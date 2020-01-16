open Femto
open Femto.ProjectCracker
open System
open System.Net.Http
open Serilog
open Npm
open FSharp.Compiler.AbstractIL.Internal.Library
open Fake.Core
open Thoth.Json.Net
open Fake.SystemHelper
open Argu

let logger = LoggerConfiguration().WriteTo.Console().CreateLogger()

type LibraryWithNpmDeps = {
    Path : string
    Name : string
    NpmDependencies : NpmDependency list
}

[<RequireQualifiedAccess>]
type NodeManager =
    | Yarn
    | Npm

    member this.CommandName =
        match this with
        | Yarn -> "yarn"
        | Npm -> "npm"

[<RequireQualifiedAccess>]
type ResolveAction =
    | Install of library:string * package:string * version:string * range: string
    | InstallDev of library:string * package:string * version:string * range: string
    | Uninstall of library:string * package: string * version:string
    | UninstallDev of library:string * package: string * version:string
    | UnableToResolve of library:string * package:string * range:string * error: string

type PackageJson = {
    Dependencies : Map<string, string> option
    DevDependencies : Map<string, string> option
}

let findLibraryWithNpmDeps (project: CrackedFsproj) =
    [ yield project.ProjectFile
      yield! project.ProjectReferences
      for package in project.PackageReferences do yield package.FsprojPath ]
    |> List.map (fun proj ->
        let npmDeps = Npm.parseDependencies (Path.normalizeFullPath proj)
        {
            Path = proj
            Name = Path.GetFileNameWithoutExtension proj
            NpmDependencies = npmDeps
        }
    )
    |> List.filter (fun projet -> not (List.isEmpty projet.NpmDependencies))

let rec findFile (fileName: string) (project: string) =
    let parentDir = IO.Directory.GetParent project
    if isNull parentDir then None
    else
      parentDir.FullName
      |> IO.Directory.GetFiles
      |> Seq.tryFind (fun file -> Path.GetFileName file = fileName)
      |> Option.orElse (findFile fileName parentDir.FullName)

/// Determines the full path of the package.json file by recursively checking every directory and it's parent starting from the path of the project file
let rec findPackageJson = findFile "package.json"

/// Determines which node package maneger to use by checking whether the yarn.lock file is present next to package.json
let workspaceCommand (packageJson: string) =
    let parentDir = IO.Directory.GetParent packageJson
    let siblings = [ yield! IO.Directory.GetFiles parentDir.FullName ]
    let yarnLockExists = siblings |> List.exists (fun file -> file.EndsWith "yarn.lock")
    if yarnLockExists
    then NodeManager.Yarn
    else NodeManager.Npm

/// Determines whether npm packages have been restored by checking the existence of node_modules directory is present next to package.json
let needsNodeModules (packageJson: string) =
    let parentDir = IO.Directory.GetParent packageJson
    let siblings = IO.Directory.GetDirectories parentDir.FullName
    let nodeModulesExists = siblings |> Array.exists (fun file -> file.EndsWith "node_modules")
    not nodeModulesExists

let findInstalledPackages (packageJson: string) : ResizeArray<InstalledNpmPackage> =
    let file = IO.File.ReadAllText packageJson
    let topLevelPackages =
        match Decode.Auto.fromString<PackageJson>(file, isCamelCase = true) with
        | Ok rawPackage ->
            let createInstalledPackages isDevDependency (dependencies: Map<string, string> option) =
                 dependencies
                 |> Option.defaultValue Map.empty
                 |> Seq.map (fun pair -> {
                    Name = pair.Key;
                    Range = Some (SemVer.Range(pair.Value));
                    Installed = None;
                    DevDependency = isDevDependency
                })
            
            ResizeArray [
                yield! createInstalledPackages false rawPackage.Dependencies;
                yield! createInstalledPackages true rawPackage.DevDependencies
            ]
        | Error errorMessage ->
            logger.Error("Couldn't find packages in 'package.json' file. Reason: {Message}", errorMessage)
            ResizeArray []

    if needsNodeModules packageJson
    then
        // should have installed node_modules
        // return just top-level packages without their exact installed versions
        topLevelPackages
    else
        // node_modules is present
        // traverse all packages in there to extract their exact versions
        // assumes node_modules is *flattened*
        let parentDir = IO.Directory.GetParent packageJson
        let siblings = IO.Directory.GetDirectories parentDir.FullName
        // using Array.find because we know for sure node_modules is present
        let nodeModulePath = siblings |> Array.find (fun dir -> dir.EndsWith "node_modules")
        for dir in IO.Directory.GetDirectories nodeModulePath do
            let pkgJson = IO.Path.Combine(dir, "package.json")
            if not (IO.File.Exists pkgJson)
                then ()
            else
                let nameAndVersionDecoder = Decode.object (fun get ->
                    (get.Required.Field "name" Decode.string,
                     get.Required.Field "version" Decode.string)
                )
                    
                let decoded =
                    File.readAllTextNonBlocking pkgJson
                    |> Decode.fromString nameAndVersionDecoder
                    
                match decoded with
                | Ok (name, version) ->
                    for package in topLevelPackages do
                        if package.Name = name
                        then package.Installed <- Some (SemVer.Version version)
                        else ()
                | Error errorMessage ->
                    logger.Error("Couldn't decode 'package.json' from {PackageJson}. Reason: {Message}", pkgJson, errorMessage)
                    ()

        topLevelPackages

module CreateProcess =
    /// Creates a cross platfrom command from the given program and arguments.
    ///
    /// For example:
    ///
    /// ```fsharp
    /// CreateProcess.xplatCommand "npm" [ "install" ]
    /// ```
    ///
    /// Will be the following on windows
    ///
    /// ```fsharp
    /// CreateProcess.fromRawCommand "cmd" [ "/C"; "npm"; "install" ]
    /// ```
    /// And the following otherwise
    ///
    /// ```fsharp
    /// CreateProcess.fromRawCommand "npm" [ "install" ]
    /// ```
    let xplatCommand program args =
        let program', args' =
            if Environment.isWindows
            then "cmd", List.concat [ [ "/C"; program ]; args ]
            else program, args

        CreateProcess.fromRawCommand program' args'

let private getPackageVersions (nodeManager : NodeManager) (pkg : NpmDependency) =
    match nodeManager with
    | NodeManager.Npm ->
        let res =
            CreateProcess.xplatCommand "npm" [ "show"; pkg.Name; "versions"; "--json" ]
            |> CreateProcess.redirectOutput
            |> Proc.run

        if res.ExitCode = 0 then
            let versions = Decode.unsafeFromString (Decode.list Decode.string) res.Result.Output
            Some versions
        else
            None

    | NodeManager.Yarn ->
        let res =
            CreateProcess.xplatCommand "yarn" [ "info"; pkg.Name; "versions"; "--json" ]
            |> CreateProcess.redirectOutput
            |> CreateProcess.ensureExitCode
            |> Proc.run
        // Yarn returns ExitCode = 0 even if the packge is not found,
        // so we need to check using StrErr channel
        if String.isNullOrEmpty res.Result.Error || res.Result.Error.Contains "ExperimentalWarning" then
            let versions = Decode.unsafeFromString (Decode.field "data" (Decode.list Decode.string)) res.Result.Output
            Some versions
        else
            None

/// Queries the available versions of a package by name and finds the first version that satisfies the version range of the dependency
let getSatisfyingPackageVersion (pkg : NpmDependency) =
    pkg.Constraint
    |> Option.bind (fun range ->
        if pkg.LowestMatching then
            pkg.Versions
            |> Seq.ofList
            |> range.Satisfying
            |> Seq.tryHead
        else
            pkg.Versions
            |> Seq.ofList
            |> range.MaxSatisfying
            |> function
                | null -> None
                | version -> Some version
    )

/// Computes which actions are required for full package resolution of a single library based on the available npm dependency metadata and the currently installed npm packages
let rec autoResolveActions
    (nodeManager : NodeManager)
    (library : LibraryWithNpmDeps)
    (packagesToVerify : NpmDependency list)
    (installedPackages : ResizeArray<InstalledNpmPackage>)
    (actions: ResolveAction list) =

    match packagesToVerify with
    | package :: rest ->
        let resolveActions =
            let installed = installedPackages |> Seq.tryFind (fun p -> p.Name = package.Name)
            match installed with
            | None ->
                // not installed -> needs to be installed
                let requiredVersion = getSatisfyingPackageVersion  package

                match requiredVersion with
                | None ->
                    let error = "Could not find a version that satisfies the required range"
                    [ ResolveAction.UnableToResolve(library.Name, package.Name, package.RawVersion, error) ]
                | Some version ->
                    if package.DevDependency then
                        [ ResolveAction.InstallDev(library.Name, package.Name, version, package.RawVersion) ]
                    else
                        [ ResolveAction.Install(library.Name, package.Name, version, package.RawVersion) ]

            | Some installedPackage ->
                // already installed -> check whether it falls under the required constraint
                match  installedPackage.Installed with
                | Some installedVersion ->
                    // check installed version satisfies package constraint
                    match package.Constraint with
                    | Some requiredRange  ->
                        if requiredRange.IsSatisfied installedVersion
                        then
                            // versions are correct, check if they are both devDepdendenicies or both dependencies
                            if package.DevDependency && not installedPackage.DevDependency then
                                [
                                    // uninstall from "dependencies"
                                    ResolveAction.Uninstall(library.Name, installedPackage.Name, installedVersion.ToString())
                                    // re-install as "devDependency"
                                    ResolveAction.InstallDev(library.Name, package.Name, installedVersion.ToString(), package.RawVersion)
                                ]
                            elif not package.DevDependency && installedPackage.DevDependency then
                                [
                                    // uninstall from "devDependencies"
                                    ResolveAction.UninstallDev(library.Name, installedPackage.Name, installedVersion.ToString())
                                    // re-install into "dependencies"
                                    ResolveAction.Install(library.Name, package.Name, installedVersion.ToString(), package.RawVersion)
                                ]
                            else
                                // both are either "devDependencies" or "dependencies"
                                // nothing left to do
                                [ ]
                        else
                            // installed version falls outside of required range
                            // resolve version from required range
                            let requiredVersion = getSatisfyingPackageVersion  package
                            match requiredVersion with
                            | Some resolvedVersion ->
                                [
                                    // uninstall current
                                    if installedPackage.DevDependency
                                    then yield ResolveAction.UninstallDev(library.Name, installedPackage.Name, installedVersion.ToString())
                                    else yield ResolveAction.Uninstall(library.Name, installedPackage.Name, installedVersion.ToString())
                                    // Re-install using the resolved version
                                    if package.DevDependency
                                    then yield ResolveAction.InstallDev(library.Name, package.Name, resolvedVersion, package.RawVersion)
                                    else yield ResolveAction.Install(library.Name, package.Name, resolvedVersion, package.RawVersion)
                                ]

                            | None ->
                                let error = "Could not find a version that satisfies the required range"
                                [ ResolveAction.UnableToResolve(library.Name, installedPackage.Name, package.RawVersion, error) ]

                    | None ->
                        let error = "Required range of npm package was not correctly parsable"
                        [ ResolveAction.UnableToResolve(library.Name, installedPackage.Name, package.RawVersion, error) ]

                | _ ->
                    [ ]

        autoResolveActions nodeManager library rest installedPackages (List.append actions resolveActions)

    | [ ] ->
        actions

/// Computes the actions required for full package resolution for a list of Fable libraries based on their npm dependency metadata and the installed npm packages
let rec autoResolve
    (nodeManager : NodeManager)
    (libraries : LibraryWithNpmDeps list)
    (installedPackages : ResizeArray<InstalledNpmPackage>)
    (resolveActions: ResolveAction list) =

    match libraries with
    | library :: rest ->
        let actions = autoResolveActions nodeManager library library.NpmDependencies installedPackages resolveActions
        autoResolve nodeManager rest installedPackages actions

    | [ ] ->
        resolveActions

type InstallArgs = {
    Package: string
    Version: string option
}

[<RequireQualifiedAccess>]
type PackageArgs =
    | Install of InstallArgs
    | Uninstall of package:string
    | DoNothing

type FemtoArgs = {
    /// The project on which the analysis is performed
    Project: string option
    /// When set to true, validates the metadata of the npm dependencies of a Fable library
    Validate: bool
    /// When set to true, performs the required actions for full package resolution
    Resolve: bool
    /// When set to true, displays the version of Femto
    DiplayVersion: bool
    LogInitialProjectAnalysis: bool
    /// The nuget package to install
    PackageArgs: PackageArgs
}

let defaultCliArgs = {
    Project = None
    Validate = false
    Resolve = false
    DiplayVersion = false
    PackageArgs = PackageArgs.DoNothing
    LogInitialProjectAnalysis = true
}

let resolveConflicts (actions: ResolveAction list) =
    // remove duplicate uninstall commands
    let distinctUninstallActions =
        actions
        |> List.choose (function
            | ResolveAction.Uninstall(library, pkg, version) -> Some(library, pkg, version, false)
            | ResolveAction.UninstallDev(library, pkg, version) -> Some (library, pkg, version, true)
            | _ -> None)
        |> List.distinctBy (fun (lib, pkg, version, isDev) -> pkg)
        |> List.map (fun (lib, pkg, version, isDev) ->
            if isDev
            then ResolveAction.UninstallDev(lib, pkg, version)
            else ResolveAction.Uninstall(lib, pkg, version))

    let distinctInstallActions =
        actions
        |> List.choose (function
            | ResolveAction.Install(lib, package, version, range) -> Some(lib, package, version, range)
            | _ -> None)
        // distinct by the combination of package, version and range
        |> List.distinctBy (fun (_, pkgName, version, range) -> pkgName, version, range)
        // group duplicate package install commands
        // try find a version that satisfies all ranges
        |> List.groupBy (fun (_, pkg, _, _) -> pkg)
        |> List.map (fun (pkgName, packages) ->
            match packages with
            | [ (lib, _, version, range) ] ->
                // make sure the library did not have an unresolvable version
                actions
                |> List.choose (function
                    | ResolveAction.UnableToResolve(lib, pkg, range, error) when pkg = pkgName -> Some(SemVer.Range(range))
                    | _ -> None)
                |> List.tryHead
                |> function
                    | None -> ResolveAction.Install(lib, pkgName, version, range)
                    | Some problematicRange ->
                        if not (problematicRange.IsSatisfied (SemVer.Version(version)))
                        then
                            let errorMsg = sprintf "Resolved version %s satisfies [%s] but not %s" version range (problematicRange.ToString())
                            ResolveAction.UnableToResolve(lib, pkgName, problematicRange.ToString(), errorMsg)
                        else
                            ResolveAction.Install(lib, pkgName, version, range)

            | multiplePackages ->
                let libName = multiplePackages |> List.map (fun (lib, _, _, _) -> lib) |> List.head
                let ranges = multiplePackages |> List.map (fun (_, _, _, range) -> SemVer.Range range)
                let versions = multiplePackages |> List.map (fun (_, _, version, _ ) -> SemVer.Version version)
                // find a version that satisfies all ranges
                versions
                |> List.tryFind (fun version -> ranges |> List.forall (fun range -> range.IsSatisfied version))
                |> function
                    | None ->
                        // could not find a version that satisfies all ranges
                        // find a sample version that doesn't satisfy a sample range and report them
                        let rangeStrings = multiplePackages |> List.map (fun (_, _, _, range) -> range)
                        let errorMsg = sprintf "Could not find a version that satisfies the ranges [%s]" (String.concat ", " rangeStrings)
                        ResolveAction.UnableToResolve(libName, pkgName, String.concat ", " rangeStrings, errorMsg)

                    | Some version ->
                        // found a version that satisfies all versions!
                        // look ranges that weren't resolved
                        let rangeStrings = multiplePackages |> List.map (fun (_, _, _, range) -> range)

                        actions
                        |> List.choose (function
                            | ResolveAction.UnableToResolve(lib, pkg, range, error) when pkg = pkgName -> Some(SemVer.Range(range))
                            | _ -> None)
                        |> List.tryHead
                        |> function
                            | None ->
                                ResolveAction.Install(libName, pkgName, version.ToString(), String.concat " && " rangeStrings)
                            | Some problematicRange ->
                                if not (problematicRange.IsSatisfied version)
                                then
                                    let errorMsg = sprintf "Resolved version %s satisfies [%s] but not %s" (version.ToString()) (String.concat " && " rangeStrings) (problematicRange.ToString())
                                    ResolveAction.UnableToResolve(libName, pkgName, problematicRange.ToString(), errorMsg)
                                else
                                    ResolveAction.Install(libName, pkgName, version.ToString(), String.concat " && " rangeStrings)
        )

    let distinctInstallDevActions =
        actions
        |> List.choose (function
            | ResolveAction.InstallDev(lib, package, version, range) -> Some(lib, package, version, range)
            | _ -> None)
        // distinct by the combination of package, version and range
        |> List.distinctBy (fun (_, pkgName, version, range) -> pkgName, version, range)
        // group duplicate package install commands
        // try find a version that satisfies all ranges
        |> List.groupBy (fun (_, pkg, _, _) -> pkg)
        |> List.map (fun (pkgName, packages) ->
            match packages with
            | [ (lib, _, version, range) ] ->
                // make sure the library was did not have an unresolvable version
                actions
                |> List.choose (function
                    | ResolveAction.UnableToResolve(lib, pkg, range, error) when pkg = pkgName -> Some(SemVer.Range(range))
                    | _ -> None)
                |> List.tryHead
                |> function
                    | None -> ResolveAction.InstallDev(lib, pkgName, version, range)
                    | Some problematicRange ->
                        if not (problematicRange.IsSatisfied (SemVer.Version(version)))
                        then
                            let errorMsg = sprintf "Resolved version %s satisfies [%s] but not %s" version range (problematicRange.ToString())
                            ResolveAction.UnableToResolve(lib, pkgName, problematicRange.ToString(), errorMsg)
                        else
                            ResolveAction.InstallDev(lib, pkgName, version, range)

            | multiplePackages ->
                let libName = multiplePackages |> List.map (fun (lib, _, _, _) -> lib) |> List.head
                let ranges = multiplePackages |> List.map (fun (_, _, _, range) -> SemVer.Range range)
                let versions = multiplePackages |> List.map (fun (_, _, version, _ ) -> SemVer.Version version)
                // find a version that satisfies all ranges
                versions
                |> List.tryFind (fun version -> ranges |> List.forall (fun range -> range.IsSatisfied version))
                |> function
                    | None ->
                        // could not find a version that satisfies all ranges
                        // find a sample version that doesn't satisfy a sample range and report them
                        let rangeStrings = multiplePackages |> List.map (fun (_, _, _, range) -> range)
                        let errorMsg = sprintf "Could not find a version that satisfies the ranges [%s]" (String.concat ", " rangeStrings)
                        ResolveAction.UnableToResolve(libName, pkgName, String.concat ", " rangeStrings, errorMsg)

                    | Some version ->
                        // found a version that satisfies all versions!
                        // look ranges that weren't resolved
                        let rangeStrings = multiplePackages |> List.map (fun (_, _, _, range) -> range)

                        actions
                        |> List.choose (function
                            | ResolveAction.UnableToResolve(lib, pkg, range, error) when pkg = pkgName -> Some(SemVer.Range(range))
                            | _ -> None)
                        |> List.tryHead
                        |> function
                            | None ->
                                ResolveAction.InstallDev(libName, pkgName, version.ToString(), String.concat " && " rangeStrings)
                            | Some problematicRange ->
                                if not (problematicRange.IsSatisfied version)
                                then
                                    let errorMsg = sprintf "Resolved version %s satisfies [%s] but not %s" (version.ToString()) (String.concat " && " rangeStrings) (problematicRange.ToString())
                                    ResolveAction.UnableToResolve(libName, pkgName, problematicRange.ToString(), errorMsg)
                                else
                                    ResolveAction.InstallDev(libName, pkgName, version.ToString(), String.concat " && " rangeStrings)
        )

    let unresolvableActions =
        actions
        |> List.filter (function
            | ResolveAction.UnableToResolve (_) -> true
            | _ -> false)

    List.concat [
        distinctUninstallActions
        distinctInstallDevActions
        distinctInstallActions
        unresolvableActions
    ]

let executeResolutionActions (cwd: string) (manager: NodeManager) (actions: ResolveAction list) =
    let actions = resolveConflicts actions

    let uninstallPackages =
        actions |> List.choose (function
        | ResolveAction.Uninstall(_, pkg, _) -> Some pkg
        | ResolveAction.UninstallDev(_, pkg, _) -> Some pkg
        | _ -> None)

    let dependenciesToInstall =
        actions |> List.choose (function
        | ResolveAction.Install(_, pkg, version, range)-> Some (pkg, version)
        | _ -> None)

    let devDependenciesToInstall =
        actions |> List.choose (function
        | ResolveAction.InstallDev(_, pkg, version, range)-> Some (pkg, version)
        | _ -> None)

    if not (List.isEmpty uninstallPackages) then
        // then there some packages we need to uninstall first
        let program, args =
            match manager with
            | NodeManager.Npm -> "npm", [ yield "uninstall"; yield! uninstallPackages ]
            | NodeManager.Yarn -> "yarn", [ yield "remove"; yield! uninstallPackages ]

        logger.Information("Uninstalling [{Libraries}]", String.concat ", " uninstallPackages)
        CreateProcess.xplatCommand program args
        |> CreateProcess.withWorkingDirectory cwd
        |> CreateProcess.ensureExitCodeWithMessage (sprintf "Error while uninstalling [%s]" (String.concat ", " uninstallPackages))
        |> CreateProcess.redirectOutput
        |> Proc.run
        |> ignore

    if not (List.isEmpty dependenciesToInstall) then
        // there are packages that need to be installed
        let packagesToInstall =
            dependenciesToInstall
            |> List.map (fun (package, version) -> sprintf "%s@%s" package version)

        let program, args =
            match manager with
            | NodeManager.Npm -> "npm", [ yield "install"; yield! packagesToInstall; yield "--save" ]
            | NodeManager.Yarn -> "yarn", [ yield "add"; yield! packagesToInstall ]

        logger.Information("Installing dependencies [{Libraries}]", String.concat ", " packagesToInstall)
        CreateProcess.xplatCommand program args
        |> CreateProcess.withWorkingDirectory cwd
        |> CreateProcess.ensureExitCodeWithMessage (sprintf "Error while installing %s" (String.concat ", " packagesToInstall))
        |> CreateProcess.redirectOutput
        |> Proc.run
        |> ignore

    if not (List.isEmpty devDependenciesToInstall) then
        // there are packages that need to be installed
        let packagesToInstall =
            devDependenciesToInstall
            |> List.map (fun (package, version) -> sprintf "%s@%s" package version)

        let program, args =
            match manager with
            | NodeManager.Npm -> "npm", [ yield "install"; yield! packagesToInstall; yield "--save-dev" ]
            | NodeManager.Yarn -> "yarn", [ yield "add"; yield! packagesToInstall; yield "--dev" ]

        logger.Information("Installing development dependencies [{Libraries}]", String.concat ", " packagesToInstall)
        CreateProcess.xplatCommand program args
        |> CreateProcess.withWorkingDirectory cwd
        |> CreateProcess.ensureExitCodeWithMessage (sprintf "Error while installing dev %s" (String.concat ", " packagesToInstall))
        |> CreateProcess.redirectOutput
        |> Proc.run
        |> ignore

    // print out resolution errors
    for action in actions do
        match action with
        | ResolveAction.UnableToResolve(library, package, version, error) ->
            logger.Error("{Library} -> Unable to resolve {Package} {Version}", library, package, version)
            logger.Error(error)

        | otherwise ->
            ignore()

let private validateProject (library : LibraryWithNpmDeps) =
    (true, library.NpmDependencies)
    ||> List.fold (fun (state: bool)  (pkg: NpmDependency) ->
        logger.Information("{Library} requires npm package {Package}", library.Name, pkg.Name)
        logger.Information("  | -- Required range {Range}", pkg.RawVersion)
        logger.Information("  | -- Resolution strategy '{Strategy}'", if pkg.LowestMatching then "Min" else "Max")

        let isCurrentPkgOk =
            match getSatisfyingPackageVersion pkg with
            | Some version ->
                logger.Information("  | -- ✔ Found version {Version} that satisfies the required range", version)
                true
            | None ->
                logger.Error("  | -- Could not find a version that satisfies the required range {Range}", pkg.RawVersion)
                false

        state && isCurrentPkgOk
    )

let previewResolutionActions
    (actions: ResolveAction list)
    (installedPackages : InstalledNpmPackage list)
    (libraries : LibraryWithNpmDeps list)
    (nodeManager: NodeManager) =

    // group resolution actions by library/F# project
    let actionsByLibrary =
        actions
        |> List.groupBy (function
            | ResolveAction.Install(lib, _, _, _) -> lib
            | ResolveAction.InstallDev(lib, _,_,_) -> lib
            | ResolveAction.Uninstall(lib, _, _) -> lib
            | ResolveAction.UninstallDev(lib, _, _) -> lib
            | ResolveAction.UnableToResolve(lib, _, _, _) -> lib)

    // libraries without required resolution action will also be logged
    // telling the user that everything is OK
    let librariesWithoutActions =
        let librariesWithActions = List.map fst actionsByLibrary
        libraries
        |> List.filter (fun lib -> not (librariesWithActions |> List.contains lib.Name))

    for library in librariesWithoutActions do
        for npmPackage in library.NpmDependencies do
            logger.Information("{Library} requires npm package {Package}", library.Name, npmPackage.Name)
            logger.Information("  | -- Required range {Range} found in project file", npmPackage.Constraint |> Option.map string |> Option.defaultValue npmPackage.RawVersion)
            let installedPackage = installedPackages |> List.tryFind (fun pkg -> pkg.Name = npmPackage.Name)
            match installedPackage with
            | None ->
                // since the library did not require actions, this will never be logged actually
                logger.Error("  | -- Missing {package} in package.json", npmPackage.Name)
            | Some package ->
                match package.Range, package.Installed with
                | Some range, Some version ->
                    logger.Information("  | -- Used range {Range} in package.json", range.ToString())
                    match npmPackage.Constraint with
                    | Some requiredRange when range.IsSatisfied version ->
                        logger.Information("  | -- ✔ Installed version {Version} satisfies required range {Range}", version.ToString(), requiredRange.ToString())
                    | _ ->
                        // since the library did not require actions, this will never be logged actually
                        logger.Error("  | -- Installed version {Version} does not satisfy required range {Range}", version.ToString(), npmPackage.Constraint |> Option.map string |> Option.defaultValue npmPackage.RawVersion)

                | _ ->
                    ignore()

    for (library, libActions) in actionsByLibrary do
        // group actions required by package
        let actionsByPackage =
            libActions
            |> List.groupBy (function
                | ResolveAction.Install(_, pkg, _, _) -> pkg
                | ResolveAction.InstallDev(_, pkg,_,_) -> pkg
                | ResolveAction.Uninstall(_, pkg, _) -> pkg
                | ResolveAction.UninstallDev(_, pkg, _) -> pkg
                | ResolveAction.UnableToResolve(_, pkg, _, _) -> pkg)

        let currentLibrary = libraries |> List.find (fun lib -> lib.Name = library)

        for (package, pkgActions) in actionsByPackage do
            // using List.find because we can assume that the dependency was part of the current library
            let requiredPackage = currentLibrary.NpmDependencies |> List.find (fun pkg -> pkg.Name = package)
            logger.Information("{Library} requires npm package {Package}", library, package)
            logger.Information("  | -- Required range {Range} found in project file", requiredPackage.Constraint |> Option.map string |> Option.defaultValue requiredPackage.RawVersion)

            let installedPackage = installedPackages |> List.tryFind (fun pkg -> pkg.Name = package)
            match installedPackage with
            | None ->
                logger.Information("  | -- Missing {package} in package.json", package)
            | Some installed ->
                match installed.Range, installed.Installed with
                | Some range, Some version ->
                    logger.Information("  | -- Used range {Range} in package.json", range.ToString())
                    logger.Information("  | -- Found installed version {Version}", version.ToString())
                | _ ->
                    ignore()

            let nodeCmd npm yarn =
                match nodeManager with
                | NodeManager.Npm -> npm
                | NodeManager.Yarn -> yarn

            // package actions one of the following
            // - Install missing package
            // - Uninstall from "devDependencies" and re-install into "dependencies"
            // - Uninstall from "dependencies" and re-install "devDependencies"
            // - Uninstall old (or "too new") package and re-install based on resolved version
            // - Unable to resolve with errors
            match pkgActions with
            | [ ResolveAction.Install(_, _, version, range) ] ->
                if range.Contains "&&"
                then logger.Information("  | -- Required version constraint from multiple projects [{Range}]", package, range)
                let installationCommand =
                    nodeCmd
                      (sprintf "npm install %s@%s --save" package version)
                      (sprintf "yarn add %s@%s" package version)
                logger.Information("  | -- Resolve manually using '{Command}'", installationCommand)

            | [ ResolveAction.InstallDev(_, _, version, range) ] ->
                if range.Contains "&&"
                then logger.Information("  | -- Required version constraint from multiple projects [{Range}]", package, range)
                let installationCommand =
                    nodeCmd
                      (sprintf "npm install %s@%s --save-dev" package version)
                      (sprintf "yarn add %s@%s --dev" package version)
                logger.Information("  | -- Resolve manually using '{Command}'", installationCommand)

            // Moving a package from "devDependencies" into "dependencies"
            // by means of un-installing it first from the project
            // then re-installing it into "dependencies"
            | [ ResolveAction.UninstallDev(_); ResolveAction.Install(_, _, version, range) ] ->
                logger.Information("  | -- {Package} was installed into \"devDependencies\" instead of \"dependencies\"", package)
                logger.Information("  | -- Re-install as a production dependency")
                if range.Contains "&&" then logger.Information("  | -- {Packge} specified from multiple projects to satisfy {Range}", package, range)
                let installationCommand =
                    nodeCmd
                      (sprintf "npm install %s@%s --save" package version)
                      (sprintf "yarn add %s@%s" package version)

                let uninstallCommand =
                    nodeCmd
                        (sprintf "npm uninstall %s" package)
                        (sprintf "yarn remove %s" package)

                logger.Information("  | -- Resolve manually using '{Uninstall}' then '{Install}'", uninstallCommand, installationCommand)

            // Moving a package from "dependencies" into "devDependencies"
            // by means of un-installing it first from the project
            // then re-installing it into "dependencies"
            | [ ResolveAction.Uninstall(_); ResolveAction.InstallDev(_, _, version, range) ] ->
                logger.Information("  | -- {Package} was installed into \"dependencies\" instead of \"devDependencies\"", package)
                logger.Information("  | -- Re-install as a development dependency")
                if range.Contains "&&" then logger.Information("  | -- {Package} specified from multiple projects to satisfy {Range}", package, range)
                let installationCommand =
                    nodeCmd
                      (sprintf "npm install %s@%s --save-dev" package version)
                      (sprintf "yarn add %s@%s --dev" package version)

                let uninstallCommand =
                    nodeCmd
                        (sprintf "npm uninstall %s" package)
                        (sprintf "yarn remove %s" package)

                logger.Information("  | -- Resolve manually using '{Uninstall}' then '{Install}'", uninstallCommand, installationCommand)

            // Modifying a package from "dependencies" into a proper version
            // that satisfies the requied range
            | [ ResolveAction.Uninstall(_, _, installedVersion); ResolveAction.Install(_, _, version, range) ] ->
                if range.Contains "&&" then logger.Information("  | -- {Packge} specified from multiple projects to satisfy {Range}", package, range)
                logger.Information("  | -- Installed version {Version} does not satisfy [{Range}]", installedVersion, range)
                let installationCommand =
                    nodeCmd
                      (sprintf "npm install %s@%s --save" package version)
                      (sprintf "yarn add %s@%s" package version)

                let uninstallCommand =
                    nodeCmd
                        (sprintf "npm uninstall %s" package)
                        (sprintf "yarn remove %s" package)

                logger.Information("  | -- Resolve manually using '{Uninstall}' then '{Install}'", uninstallCommand, installationCommand)

            // Modifying a package from "devDependencies" into a proper version
            // that satisfies the requied range
            | [ ResolveAction.UninstallDev(_, _, installedVersion); ResolveAction.InstallDev(_, _, version, range) ] ->
                if range.Contains "&&" then logger.Information("  | -- {Packge} specified from multiple projects to satisfy {Range}", package, range)
                logger.Information("  | -- Installed version {Version} does not satisfy [{Range}]", installedVersion, range)
                let installationCommand =
                    nodeCmd
                      (sprintf "npm install %s@%s --save-dev" package version)
                      (sprintf "yarn add %s@%s --dev" package version)

                let uninstallCommand =
                    nodeCmd
                        (sprintf "npm uninstall %s" package)
                        (sprintf "yarn remove %s" package)

                logger.Information("  | -- Resolve manually using '{Uninstall}' then '{Install}'", uninstallCommand, installationCommand)

            // UnableToResolve can come in pairs: one original and one derived
            | [ ResolveAction.UnableToResolve(_, _,_ , error); ResolveAction.UnableToResolve(_) ] ->
                logger.Error("  | -- " + error)

            // UnableToResolve can come in pairs: one original and one derived
            // but sometimes an obsolete "uninstall" action will stay behind as well
            | [ (ResolveAction.Uninstall(_) | ResolveAction.UninstallDev(_)); ResolveAction.UnableToResolve(_, _,_ , error); ResolveAction.UnableToResolve(_) ] ->
                logger.Error("  | -- " + error)

            | [ ResolveAction.UnableToResolve(_, _, _, error) ] ->
                logger.Error("  | -- " + error)

            | _ ->
                ignore()

let private runValidate (libraries : LibraryWithNpmDeps list) =
    logger.Information("Ensuring project can be analyzed")

    let isValid =
        (true, libraries)
        ||> List.fold (fun (state : bool) (library : LibraryWithNpmDeps) ->
            validateProject library && state
        )

    if isValid then
        logger.Information("Validation result: Success")
        FemtoResult.ValidationSucceeded
    else
        logger.Error("Validation result: Failed")
        FemtoResult.ValidationFailed

let private restoreNodeModules (packageJson : string) (nodeManager : NodeManager) =
    let packageJsonDir = (IO.Directory.GetParent packageJson).FullName

    logger.Information("Found package.json in {Dir}", packageJsonDir)
    if needsNodeModules packageJson then
        logger.Information("Npm packages need to be restored first for project analysis")
        let restoreCommand = sprintf "%s install" nodeManager.CommandName
        logger.Information("Restoring npm packages using '{Command}' inside {Dir}", restoreCommand, packageJsonDir)
        CreateProcess.xplatCommand nodeManager.CommandName [ "install" ]
        |> CreateProcess.withWorkingDirectory packageJsonDir
        |> CreateProcess.redirectOutput
        |> CreateProcess.ensureExitCode
        |> Proc.run
        |> ignore

let private runResolution (resolve : bool) (packageJson : string option) (nodeManager : NodeManager) (libraries : LibraryWithNpmDeps list) =
    match packageJson with
    | None ->
        for library in libraries do
            for pkg in library.NpmDependencies do
                logger.Information("{Library} requires npm package {Package} ({Version})", library.Name, pkg.Name, pkg.RawVersion)

        logger.Warning "Could not locate package.json file"
        FemtoResult.MissingPackageJson

    | Some packageJson ->

        restoreNodeModules packageJson nodeManager

        let installedPackages = findInstalledPackages packageJson

        if not resolve then
            let resolveActions = autoResolve nodeManager libraries installedPackages []
            let simplifiedActions = resolveConflicts resolveActions
            previewResolutionActions simplifiedActions (List.ofSeq installedPackages) libraries nodeManager
            let allResolutionActionsCanExecute =
                simplifiedActions
                |> List.exists (function
                    | ResolveAction.UnableToResolve(_) -> true
                    | _ -> false)
                |> not

            if allResolutionActionsCanExecute
            then FemtoResult.ValidationSucceeded
            else FemtoResult.ValidationFailed

        else
            let resolveActions = autoResolve nodeManager libraries installedPackages []
            if List.isEmpty resolveActions then
                logger.Information("✔ Required packages are already resolved")
                FemtoResult.ValidationSucceeded
            else
                logger.Information("Executing required actions for package resolution")
                try
                    let cwd = (IO.Directory.GetParent packageJson).FullName
                    executeResolutionActions cwd nodeManager resolveActions
                    logger.Information("✔ Package resolution complete")
                    FemtoResult.ValidationSucceeded
                with
                | ex ->
                    logger.Error(ex.Message)
                    FemtoResult.UnexpectedError

/// Returns whether paket is installed as a global dotnet tool
let isPaketInstalledGlobally() =
    [ "tool"; "list"; "--global" ]
    |> CreateProcess.xplatCommand "dotnet"
    |> CreateProcess.redirectOutput
    |> Proc.run
    |> fun result ->
        if result.ExitCode <> 0 then
            false
        else
            result.Result.Output
            |> String.split '\n'
            |> Seq.exists (fun line -> line.StartsWith "paket")

/// Returns whether paket is installed a local dotnet CLI tool
/// This function checks whether the entry "paket" exists in { "tools": [tool entries] }
/// The contents of the JSON is taken from .config/dotnet-tools.json which is the tool manifest
/// for local CLI tools in .NET Core 3 onwards
let isPaketInstalledAsLocalCliTool (paketDependenciesWorkingDir: string) =
    try
        let toolsConfigPath = IO.Path.Combine(paketDependenciesWorkingDir, ".config", "dotnet-tools.json")
        if IO.File.Exists toolsConfigPath then
            let toolsContentDecoded =
                IO.File.ReadAllText toolsConfigPath
                |> Decode.fromString (Decode.at ["tools"; "paket"] Decode.value)
            
            match toolsContentDecoded with
            | Ok _ -> true
            | Error _ -> false
        else
            false
    with
    | ex -> false

let installLocalCliTool name workingDir =
    CreateProcess.xplatCommand "dotnet" [ "tool"; "install"; name ]
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.redirectOutput
    |> Proc.run
    |> fun processInfo ->
        if processInfo.ExitCode <> 0 then
            Error processInfo.Result.Output
        else
            Ok()

/// Creates a .config/dotnet-tools.json file if it does not exist
/// This is a tool manifest file which allows us to install paket a local cli tool (starting from .NET Core 3)
/// TODO: Discuss whether it is a good idea to install paket automatically
let installPaketAsLocalCliToll projectRoot =
    let directory = IO.Path.Combine(projectRoot, ".config")
    let configFilePath = IO.Path.Combine(projectRoot, ".config", "dotnet-tools.json")
    if not (IO.Directory.Exists directory) then
        IO.Directory.CreateDirectory directory |> ignore
        let content = "{ \"version\":1, \"isRoot\":true, \"tools\": { } }"
        IO.File.WriteAllText(configFilePath, content)
        installLocalCliTool "paket" projectRoot
    else
        if IO.File.Exists configFilePath then
            installLocalCliTool "paket" projectRoot
        else
            let content = "{ \"version\":1, \"isRoot\":true, \"tools\": { } }"
            IO.File.WriteAllText(configFilePath, content)
            installLocalCliTool "paket" projectRoot

let installPaketFromBootstrapper projectRoot =
    let installation = async {
        try
            use httpClient = new HttpClient()
            let! paketSearchResult = Async.AwaitTask (httpClient.GetStringAsync("https://azuresearch-usnc.nuget.org/query?q=Paket&prerelease=false"))
            
            let dataDecoder =
                Decode.object (fun it ->
                    (it.Required.Field "id" Decode.string,
                     it.Required.Field "version" Decode.string)
                )
                |> Decode.list
                |> Decode.field "data"
                
            let latestPaketVersion =
                match Decode.fromString dataDecoder paketSearchResult with
                | Ok packages ->
                    packages
                    |> Seq.tryFind (fun (id, _) -> id = "Paket")
                    |> Option.map (fun (_, version) -> version)
                | Error errorMessage ->
                    logger.Error("Couldn't decode nuget packages. Reason: {Message}", errorMessage)
                    None

            match latestPaketVersion with
            | None ->
                return false
            | Some version ->
                let downloadUrl = sprintf "https://github.com/fsprojects/Paket/releases/download/%s/paket.bootstrapper.exe" version
                logger.Information("Found {Manager} version {Version}", "paket", version)
                logger.Information("Downloading bootstrapper from {Link}", downloadUrl)
                let! bootstrapperContents = Async.AwaitTask (httpClient.GetByteArrayAsync(downloadUrl))
                if not (IO.Directory.Exists (IO.Path.Combine(projectRoot, ".paket"))) then
                    IO.Path.Combine(projectRoot, ".paket")
                    |> IO.Directory.CreateDirectory
                    |> ignore

                if IO.File.Exists (IO.Path.Combine(projectRoot, ".paket", "paket.exe")) then
                    IO.File.Delete (IO.Path.Combine(projectRoot, ".paket", "paket.exe"))

                IO.File.WriteAllBytes(IO.Path.Combine(projectRoot, ".paket", "paket.exe"), bootstrapperContents)
                return true
        with
        | error ->
            return false
    }

    Async.RunSynchronously installation

let isDotnetSdkThree() =
    CreateProcess.xplatCommand "dotnet" [ "--version" ]
    |> CreateProcess.redirectOutput
    |> Proc.run
    |> fun processInfo ->
        if processInfo.ExitCode <> 0 then
            false
        else
            try
              let dotnetVersion = Version.Parse(processInfo.Result.Output)
              dotnetVersion >= Version.Parse("3.0.100")
            with
            | _ -> false

let rec private installPackage (project: string) (installArgs: InstallArgs) (originalArgs: FemtoArgs) =
    let projectDir = IO.Directory.GetParent(project)
    let projectWorkingDir = projectDir.FullName
    let filesNextToProject = IO.Directory.GetFiles projectWorkingDir
    let paketReferences =
        filesNextToProject
        |> Seq.tryFind (fun file -> file.EndsWith "paket.references")
        |> Option.map IO.File.ReadAllLines

    match paketReferences with
    | None ->
        match installArgs with
        | { Package = package; Version = None } ->
            logger.Information("Using {Manager} to install {Package}", "nuget", installArgs.Package)

        | { Package = package; Version = Some version } ->
            logger.Information("Using {Manager} to install {Package} version {Version}", "nuget", installArgs.Package, version)

        let installArguments = [
            yield "add"
            yield "package"
            yield installArgs.Package
            match installArgs.Version with
            | Some version ->
                yield "--version"
                yield version
            | None ->
                ()
        ]

        let installationResult =
            CreateProcess.xplatCommand "dotnet" installArguments
            |> CreateProcess.withWorkingDirectory projectWorkingDir
            |> CreateProcess.redirectOutput
            |> Proc.run

        if installationResult.ExitCode <> 0 then
            logger.Error("Could not install {Package}", installArgs.Package)
            let errorCommand = sprintf "%s> %s %s" projectWorkingDir "dotnet" (String.concat " " installArguments)
            logger.Error(errorCommand)
            logger.Error("Process Ouput:")
            logger.Error(installationResult.Result.Output)
            FemtoResult.NugetInstallationFailed
        else
            logger.Information("✔ Nuget package {Feliz} installed successfully", installArgs.Package)
            logger.Information("Resolving potentially required npm package with {command}", "femto --resolve")
            runner { originalArgs with PackageArgs = PackageArgs.DoNothing; Resolve = true }

    | Some references ->
        logger.Information("Detected {References} file -> use {Manager}", "paket.references", "paket")

        let group =
            references
            |> Seq.tryFind (fun line -> line.ToLower().StartsWith "group")
            |> Option.map (fun line -> line.Replace("group", "").Trim())
            |> Option.defaultValue "Main"

        match installArgs with
        | { Package = _; Version = None } ->
            logger.Information("Installing {Package} within dependency group {Group}",  installArgs.Package, group)

        | { Package = _; Version = Some version } ->
            logger.Information("Installing {Package} version {Version} within dependency group {Group}",  installArgs.Package, version, group)

        match findFile "paket.dependencies" project with
        | None ->
            FemtoResult.PaketInstallationFailedNoPaketDependencies
        | Some paketDependencies ->
            let paketInstallArgs =  [
                yield "add"
                yield installArgs.Package
                yield "--project"
                yield project
                yield "--group"
                yield group

                match installArgs.Version with
                | Some version ->
                    yield "--version"
                    yield version
                | None ->
                    ()
            ]

            let paketDependenciesParent = IO.Directory.GetParent paketDependencies
            let projectRoot = paketDependenciesParent.FullName
            // check if {projectRoot}/.paket/paket.exe and run it from Windows
            if IO.File.Exists (IO.Path.Combine(projectRoot, ".paket", "paket.exe")) && Environment.isWindows then
                logger.Information("Using locally installed {Manager} within {Path}", "paket", ".paket/paket.exe")
                paketInstallArgs
                |> CreateProcess.fromRawCommand (IO.Path.Combine(projectRoot, ".paket", "paket.exe"))
                |> CreateProcess.withWorkingDirectory projectRoot
                |> CreateProcess.redirectOutput
                |> Proc.run
                |> fun installProcess ->
                    if installProcess.ExitCode = 0
                    then
                        logger.Information("✔ Nuget package {Package} installed successfully", installArgs.Package)
                        logger.Information("Resolving potentially required npm packages with {command}", "femto --resolve")
                        runner { originalArgs with PackageArgs = PackageArgs.DoNothing; Resolve = true }
                    else
                        let erroredShellCommand =
                            sprintf "%s> %s %s"
                              // workding directory
                              projectRoot
                              // program
                              (IO.Path.Combine(projectRoot, ".paket", "paket.exe"))
                              // args
                              (String.concat " " paketInstallArgs)
                        logger.Error("Error while running the following command:")
                        logger.Error(erroredShellCommand)
                        logger.Error("Process Output: {Output}", installProcess.Result.Output)
                        logger.Error("Process Error: {Error}", installProcess.Result.Error)
                        FemtoResult.PaketFailed
            elif IO.File.Exists (IO.Path.Combine(projectRoot, ".paket", "paket.exe")) && not Environment.isWindows then
                // TODO: Check if mono exists first
                // Check if {projectRoot}/.paket/paket.exe exists and run it from non-Windows machines using mono
                logger.Information("Using locally installed {Manager} within {Path}", "paket", ".paket/paket.exe")
                List.append [ IO.Path.Combine(projectRoot, ".paket", "paket.exe") ] paketInstallArgs
                |> CreateProcess.xplatCommand "mono"
                |> CreateProcess.withWorkingDirectory projectRoot
                |> CreateProcess.redirectOutput
                |> Proc.run
                |> fun installProcess ->
                    if installProcess.ExitCode = 0
                    then
                        logger.Information("✔ Nuget package {Package} installed successfully", installArgs.Package)
                        logger.Information("Resolving potentially required npm packages with {command}", "femto --resolve")
                        runner { originalArgs with PackageArgs = PackageArgs.DoNothing; Resolve = true }
                    else
                        let erroredShellCommand =
                            sprintf "%s> %s %s"
                              // workding directory
                              projectWorkingDir
                              // program
                              "mono"
                              // args
                              (String.concat " " ( List.append [ IO.Path.Combine(projectRoot, ".paket", "paket.exe") ] paketInstallArgs))
                        logger.Error("Error while running the following command:")
                        logger.Error(erroredShellCommand)
                        logger.Error("Process Output: {Output}", installProcess.Result.Output)
                        logger.Error("Process Error: {Error}", installProcess.Result.Error)
                        FemtoResult.PaketFailed
            elif isPaketInstalledAsLocalCliTool projectRoot then
                logger.Information("Using locally installed {Manager}", "paket")
                paketInstallArgs
                |> CreateProcess.xplatCommand "dotnet paket"
                |> CreateProcess.withWorkingDirectory projectRoot
                |> CreateProcess.redirectOutput
                |> Proc.run
                |> fun installProcess ->
                    if installProcess.ExitCode = 0
                    then
                        logger.Information("✔ Nuget package {Package} installed successfully", installArgs.Package)
                        logger.Information("Resolving potentially required npm packages with {command}", "femto --resolve")
                        runner { originalArgs with PackageArgs = PackageArgs.DoNothing; Resolve = true }
                    else
                        let erroredShellCommand =
                            sprintf "%s> %s %s"
                              // workding directory
                              projectWorkingDir
                              // program
                              "dotnet paket"
                              // args
                              (String.concat " " paketInstallArgs)
                        logger.Error("Error while running the following command:")
                        logger.Error(erroredShellCommand)
                        logger.Error("Process Output: {Output}", installProcess.Result.Output)
                        logger.Error("Process Error: {Error}", installProcess.Result.Error)
                        FemtoResult.PaketFailed
            elif isPaketInstalledGlobally() then
                logger.Information("Using globally installed {Manager}", "paket")
                paketInstallArgs
                |> CreateProcess.xplatCommand "paket"
                |> CreateProcess.withWorkingDirectory projectRoot
                |> CreateProcess.redirectOutput
                |> Proc.run
                |> fun installProcess ->
                    if installProcess.ExitCode = 0
                    then
                        logger.Information("✔ Package {Package} installed successfully", installArgs.Package)
                        logger.Information("Resolving potentially required npm packages with {command}", "femto --resolve")
                        runner { originalArgs with PackageArgs = PackageArgs.DoNothing; Resolve = true }
                    else
                        let erroredShellCommand =
                            sprintf "%s> %s %s"
                              // workding directory
                              projectWorkingDir
                              // program
                              "paket"
                              // args
                              (String.concat " " paketInstallArgs)
                        logger.Error("Error while running the following command:")
                        logger.Error(erroredShellCommand)
                        logger.Error("Process Output: {Output}", installProcess.Result.Output)
                        logger.Error("Process Error: {Error}", installProcess.Result.Error)
                        FemtoResult.PaketFailed
            else
                if installPaketFromBootstrapper projectRoot then
                    runner { originalArgs with PackageArgs = PackageArgs.DoNothing; Resolve = true }
                else
                    FemtoResult.PaketNotFound

and private uninstallPackage (project: string) (package: string) =
    let projectDir = IO.Directory.GetParent(project)
    let projectWorkingDir = projectDir.FullName
    let filesNextToProject = IO.Directory.GetFiles projectWorkingDir
    let paketReferences =
        filesNextToProject
        |> Seq.tryFind (fun file -> file.EndsWith "paket.references")
        |> Option.map IO.File.ReadAllLines

    match paketReferences with
    | None ->
        logger.Information("Using {Manager} to uninstall {Package}", "nuget", package)

        let installationResult =
            CreateProcess.xplatCommand "dotnet" [ "remove"; "package"; package ]
            |> CreateProcess.withWorkingDirectory projectWorkingDir
            |> CreateProcess.redirectOutput
            |> Proc.run

        if installationResult.ExitCode <> 0 then
            logger.Error("Could not uninstall {Package}", package)
            let errorCommand = sprintf "%s> %s %s" projectWorkingDir "dotnet" (String.concat " " [ "remove"; "package"; package ])
            logger.Error(errorCommand)
            logger.Error("Process Ouput:")
            logger.Error(installationResult.Result.Output)
            FemtoResult.NugetInstallationFailed
        else
            logger.Information("✔ Nuget package {Feliz} was uninstalled", package)
            FemtoResult.PackageUninstalled

    | Some references ->
        logger.Information("Detected {References} file -> use {Manager}", "paket.references", "paket")

        let group =
            references
            |> Seq.tryFind (fun line -> line.ToLower().StartsWith "group")
            |> Option.map (fun line -> line.Replace("group", "").Trim())
            |> Option.defaultValue "Main"

        logger.Information("Uninstalling {Package} from dependency group {Group}",  package, group)

        match findFile "paket.dependencies" project with
        | None ->
            FemtoResult.PaketInstallationFailedNoPaketDependencies
        | Some paketDependencies ->
            let paketInstallArgs =  [
                "remove"
                package
                "--project"
                project
                "--group"
                group
            ]

            let paketDependenciesParent = IO.Directory.GetParent paketDependencies
            let projectRoot = paketDependenciesParent.FullName
            // check if {projectRoot}/.paket/paket.exe and run it from Windows
            if IO.File.Exists (IO.Path.Combine(projectRoot, ".paket", "paket.exe")) && Environment.isWindows then
                logger.Information("Using locally installed {Manager} within {Path}", "paket", ".paket/paket.exe")
                paketInstallArgs
                |> CreateProcess.fromRawCommand (IO.Path.Combine(projectRoot, ".paket", "paket.exe"))
                |> CreateProcess.withWorkingDirectory projectRoot
                |> CreateProcess.redirectOutput
                |> Proc.run
                |> fun installProcess ->
                    if installProcess.ExitCode = 0
                    then
                        logger.Information("✔ Nuget package {Package} was uninstalled", package)
                        FemtoResult.PackageUninstalled
                    else
                        let erroredShellCommand =
                            sprintf "%s> %s %s"
                              // workding directory
                              projectRoot
                              // program
                              (IO.Path.Combine(projectRoot, ".paket", "paket.exe"))
                              // args
                              (String.concat " " paketInstallArgs)
                        logger.Error("Error while running the following command:")
                        logger.Error(erroredShellCommand)
                        logger.Error("Process Output: {Output}", installProcess.Result.Output)
                        logger.Error("Process Error: {Error}", installProcess.Result.Error)
                        FemtoResult.PaketFailed
            elif IO.File.Exists (IO.Path.Combine(projectRoot, ".paket", "paket.exe")) && not Environment.isWindows then
                // TODO: Check if mono exists first
                // Check if {projectRoot}/.paket/paket.exe exists and run it from non-Windows machines using mono
                logger.Information("Using locally installed {Manager} within {Path}", "paket", ".paket/paket.exe")
                List.append [ IO.Path.Combine(projectRoot, ".paket", "paket.exe") ] paketInstallArgs
                |> CreateProcess.xplatCommand "mono"
                |> CreateProcess.withWorkingDirectory projectRoot
                |> CreateProcess.redirectOutput
                |> Proc.run
                |> fun installProcess ->
                    if installProcess.ExitCode = 0
                    then
                        logger.Information("✔ Nuget package {Package} was uninstalled", package)
                        FemtoResult.PackageUninstalled
                    else
                        let erroredShellCommand =
                            sprintf "%s> %s %s"
                              // workding directory
                              projectWorkingDir
                              // program
                              "mono"
                              // args
                              (String.concat " " ( List.append [ IO.Path.Combine(projectRoot, ".paket", "paket.exe") ] paketInstallArgs))
                        logger.Error("Error while running the following command:")
                        logger.Error(erroredShellCommand)
                        logger.Error("Process Output: {Output}", installProcess.Result.Output)
                        logger.Error("Process Error: {Error}", installProcess.Result.Error)
                        FemtoResult.PaketFailed
            elif isPaketInstalledAsLocalCliTool projectRoot then
                logger.Information("Using locally installed {Manager}", "paket")
                paketInstallArgs
                |> CreateProcess.xplatCommand "dotnet paket"
                |> CreateProcess.withWorkingDirectory projectRoot
                |> CreateProcess.redirectOutput
                |> Proc.run
                |> fun installProcess ->
                    if installProcess.ExitCode = 0
                    then
                        logger.Information("✔ Nuget package {Package} was unintalled", package)
                        FemtoResult.PackageUninstalled
                    else
                        let erroredShellCommand =
                            sprintf "%s> %s %s"
                              // workding directory
                              projectWorkingDir
                              // program
                              "dotnet paket"
                              // args
                              (String.concat " " paketInstallArgs)
                        logger.Error("Error while running the following command:")
                        logger.Error(erroredShellCommand)
                        logger.Error("Process Output: {Output}", installProcess.Result.Output)
                        logger.Error("Process Error: {Error}", installProcess.Result.Error)
                        FemtoResult.PaketFailed
            elif isPaketInstalledGlobally() then
                logger.Information("Using globally installed {Manager}", "paket")
                paketInstallArgs
                |> CreateProcess.xplatCommand "paket"
                |> CreateProcess.withWorkingDirectory projectRoot
                |> CreateProcess.redirectOutput
                |> Proc.run
                |> fun installProcess ->
                    if installProcess.ExitCode = 0
                    then
                        logger.Information("✔ Nuget package {Package} was uninstalled", package)
                        FemtoResult.PackageUninstalled
                    else
                        let erroredShellCommand =
                            sprintf "%s> %s %s"
                              // workding directory
                              projectWorkingDir
                              // program
                              "paket"
                              // args
                              (String.concat " " paketInstallArgs)
                        logger.Error("Error while running the following command:")
                        logger.Error(erroredShellCommand)
                        logger.Error("Process Output: {Output}", installProcess.Result.Output)
                        logger.Error("Process Error: {Error}", installProcess.Result.Error)
                        FemtoResult.PaketFailed
            else
                FemtoResult.PaketNotFound

and private runner (args : FemtoArgs) =
    if args.DiplayVersion then
        printfn "%s" Version.VERSION
        FemtoResult.ValidationSucceeded
    else
        // BEGIN: Mutualized context resolution
        match args.Project with
        | None ->
            logger.Error("Project path was not correctly provided")
            FemtoResult.ProjectFileNotFound

        | Some project ->
            let projectWorkingDir = (IO.Directory.GetParent project).FullName
            if args.LogInitialProjectAnalysis then
              logger.Information("Analyzing project {Project}", project)
              logger.Information("Running {Command} against the project", "dotnet restore")

            let restoreResult =
                let processOutput =
                    CreateProcess.xplatCommand "dotnet" [ "restore" ]
                    |> CreateProcess.withWorkingDirectory projectWorkingDir
                    |> CreateProcess.redirectOutput
                    |> Proc.run

                if processOutput.ExitCode <> 0
                then Error processOutput.Result.Output
                else Ok ()

            match restoreResult with
            | Error error ->
                if error.Contains "\"paket.exe\"' is not recognized as an internal or external command" then
                    match findFile "paket.dependencies" project with
                    | None ->
                        logger.Error("{Command} Failed with error {Error}", "dotnet restore", error)
                        FemtoResult.ValidationFailed
                    | Some paketDeps ->
                        let paketDepsParent = IO.Directory.GetParent paketDeps
                        if not (isPaketInstalledAsLocalCliTool paketDepsParent.FullName)
                            then logger.Information("Installing paket locally...")
                        if isDotnetSdkThree() then
                            match installPaketAsLocalCliToll paketDepsParent.FullName with
                            | Error paketInstallError ->
                                logger.Error("Could not install paket locally as a dotnet cli tool")
                                logger.Error(paketInstallError)
                                FemtoResult.PaketNotFound
                            | Ok() ->
                                // do initial restore from Paket
                                // first restore solution to update the restore targets
                                let paketRestoreResult =
                                    CreateProcess.xplatCommand "dotnet" [ "paket"; "restore" ]
                                    |> CreateProcess.withWorkingDirectory paketDepsParent.FullName
                                    |> CreateProcess.redirectOutput
                                    |> Proc.run
                                    |> fun processInfo ->
                                        if processInfo.ExitCode <> 0
                                        then Error processInfo.Result.Output
                                        else
                                        CreateProcess.xplatCommand "dotnet" [ "paket"; "restore"; "--project"; project ]
                                        |> CreateProcess.withWorkingDirectory paketDepsParent.FullName
                                        |> CreateProcess.redirectOutput
                                        |> Proc.run
                                        |> fun processInfo ->
                                            if processInfo.ExitCode <> 0
                                            then Error processInfo.Result.Output
                                            else Ok()

                                match paketRestoreResult with
                                | Error error ->
                                    logger.Error("{Command} failed with error {Error}", "dotnet paket restore", error)
                                    FemtoResult.PaketFailed
                                | Ok() ->
                                    runner { args with LogInitialProjectAnalysis = false }
                        else
                            if installPaketFromBootstrapper paketDepsParent.FullName then
                                logger.Information("✔ Paket was installed successfully, restarting...")
                                runner { args with LogInitialProjectAnalysis = false }
                            else
                                logger.Error("{Command} Failed with error {Error}", "dotnet restore", error)
                                FemtoResult.ValidationFailed
                else
                    logger.Error("{Command} Failed with error {Error}", "dotnet restore", error)
                    FemtoResult.ValidationFailed

            | Ok () ->
                match args.PackageArgs with
                | PackageArgs.Install installArgs -> installPackage project installArgs args
                | PackageArgs.Uninstall package -> uninstallPackage project package
                | PackageArgs.DoNothing ->
                let crackResult =
                    try
                        let projectInfo = ProjectCracker.fullCrack project
                        Ok projectInfo
                    with
                    | ex -> Error ex.Message

                match crackResult with
                | Error er ->
                    logger.Error("Error while analyzing the project's structure and dependencies")
                    FemtoResult.ProjectCrackerFailed

                | Ok crackedProject ->
                    let packageJson = findPackageJson project

                    let nodeManager =
                        packageJson
                        |> Option.map workspaceCommand
                        // TODO: Try detect if yarn is available
                        // If yes, use yarn as resolution of version is 2 times faster
                        |> Option.defaultValue NodeManager.Npm

                    logger.Information("Using {Manager} for package management", nodeManager.CommandName)

                    let libraries =
                        findLibraryWithNpmDeps crackedProject
                        |> List.map (fun library ->
                            let npmDependencies =
                                library.NpmDependencies
                                |> List.map (fun pkg ->
                                    match getPackageVersions nodeManager pkg with
                                    | Some versions ->
                                        { pkg with Versions = versions }
                                    | None ->
                                        { pkg with IsFetchOk = false }
                                )

                            { library with NpmDependencies = npmDependencies}
                        )

                    let hasInvalidNpmDepencies =
                        libraries
                        |> List.filter (fun library ->
                            library.NpmDependencies
                            |> List.exists (fun pkg ->
                                if pkg.IsFetchOk then
                                    false
                                else
                                    logger.Error("Failed to retrieve information about {Package} required by {Library}", pkg.Name, library.Name)
                                    true
                            )
                        )

                    // If we can't fetch the info a package, we stop here
                    if List.isEmpty hasInvalidNpmDepencies then
                        // END: Mutualized context resolution
                        if args.Validate then
                            runValidate libraries
                        else
                            runResolution args.Resolve packageJson  nodeManager libraries
                    else
                        FemtoResult.ValidationFailed

type CLIArguments =
    | [<MainCommand>] Project of path : string
    | Validate
    | Resolve
    | Version of string option
    | [<CliPrefix(CliPrefix.None)>] Install of package:string
    | [<CliPrefix(CliPrefix.None)>] Uninstall of package:string


    interface IArgParserTemplate with

        member this.Usage =
            match this with
            | Project _ -> "specify the path to the F# project."
            | Validate -> "check that the XML tags used in the F# project file are parsable and a npm package version can be calculated."
            | Resolve -> "resolve and install required packages."
            | Version _ -> "display the current version of Femto."
            | Install _ -> "install a package into a project"
            | Uninstall _ -> "uninstall a package from the project"

let parseArgs (cliArgs : CLIArguments list) =
    let rec apply (cliArgs : CLIArguments list) (res : FemtoArgs) =
        match cliArgs with
        | Project project :: rest ->
            let project =
                if IO.Directory.Exists project then
                    IO.Directory.GetFiles project
                    |> Seq.tryFind (fun file -> file.EndsWith ".fsproj")
                    |> Option.map Path.normalizeFullPath
                else

                    if IO.Path.IsPathRooted project then
                        Some project
                    else
                        Some (IO.Path.GetFullPath project)

            apply rest { res with Project = project }

        | Validate :: rest ->
            apply rest { res with Validate = true }

        | Resolve :: rest ->
            apply rest { res with Resolve = true }

        | Version None :: rest ->
            apply rest { res with DiplayVersion = true }

        | Version (Some version) :: rest ->
            let packageArgs =
                match res.PackageArgs with
                | PackageArgs.Install installArgs ->
                    { installArgs with Version = Some version }
                    |> PackageArgs.Install

                | otherCommand -> otherCommand

            apply rest { res with  PackageArgs = packageArgs }

        | Install package :: rest ->
            let installArgs =
                match res.PackageArgs with
                | PackageArgs.Install args -> PackageArgs.Install { args with Package = package }
                | otherwise -> PackageArgs.Install { Package = package; Version = None }

            apply rest { res with PackageArgs = installArgs }

        | Uninstall package :: rest ->
            apply rest { res with PackageArgs = PackageArgs.Uninstall package }

        | [ ] ->
            res

    let cwd = Environment.CurrentDirectory
    let siblings = IO.Directory.GetFiles cwd
    // By default, we try to find the fsproj in the currect directory
    // This can be overriden if the user passes the project path
    let defaultArgs =
        let projectFile = siblings |> Seq.tryFind (fun f -> f.EndsWith ".fsproj")
        { defaultCliArgs with Project = projectFile }

    apply cliArgs defaultArgs

[<EntryPoint>]
let main argv =
    // make sure the check marks appear as they should
    // using standard utf-8 encoding
    Console.OutputEncoding <- Text.Encoding.UTF8

    let parser = ArgumentParser.Create<CLIArguments>("femto")

    let printUsage() =
        parser.PrintUsage()
        |> printfn "%s" // We don't use the logger in order to avoid the [time type] prefix in the console

    let result =
        let args = parser.Parse(argv, raiseOnUsage = false)

        if args.IsUsageRequested then
            printUsage()
            FemtoResult.UsageRequested
        else
            args.GetAllResults()
            |> parseArgs
            |> runner

    int result
