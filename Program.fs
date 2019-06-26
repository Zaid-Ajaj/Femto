open Femto
open Femto.ProjectCracker
open System
open Serilog
open Npm
open Newtonsoft.Json.Linq
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
type ResolveAction =
    | Install of library:string * package:string * version:string * range: string
    | InstallDev of library:string * package:string * version:string * range: string
    | Uninstall of library:string * package: string * version:string
    | UninstallDev of library:string * package: string * version:string
    | UnableToResolve of library:string * package:string * range:string * error: string

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

/// Determines the full path of the package.json file by recursively checking every directory and it's parent starting from the path of the project file
let rec findPackageJson (project: string) =
    let parentDir = IO.Directory.GetParent project
    if isNull parentDir then None
    else
      parentDir.FullName
      |> IO.Directory.GetFiles
      |> Seq.tryFind (fun file -> Path.GetFileName file = "package.json")
      |> Option.orElse (findPackageJson parentDir.FullName)

[<RequireQualifiedAccess>]
type NodeManager =
    | Yarn
    | Npm

    member this.CommandName =
        match this with
        | Yarn -> "yarn"
        | Npm -> "npm"

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
    let content = JObject.Parse(IO.File.ReadAllText packageJson)
    let dependencies : JProperty list = [
        if content.ContainsKey "dependencies"
        then yield! (content.["dependencies"] :?> JObject).Properties() |> List.ofSeq
    ]

    let devDependencies = [
        if content.ContainsKey "devDependencies"
        then yield! (content.["devDependencies"] :?> JObject).Properties() |> List.ofSeq
    ]

    let topLevelPackages = ResizeArray [
        yield! [
            for package in dependencies -> {
                Name = package.Name;
                Range = Some (SemVer.Range(package.Value.ToObject<string>()));
                Installed = None
                DevDependency = false
            }
        ]

        yield! [
            for package in devDependencies -> {
                Name = package.Name;
                Range = Some (SemVer.Range(package.Value.ToObject<string>()));
                Installed = None
                DevDependency = true
            }
        ]

    ]

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
                let pkgJsonContent = JObject.Parse(File.readAllTextNonBlocking pkgJson)
                for pkg in topLevelPackages do
                    if pkg.Name = pkgJsonContent.["name"].ToObject<string>()
                    then pkg.Installed <- Some (SemVer.Version (pkgJsonContent.["version"].ToObject<string>()))
                    else ()

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

/// Queries the available versions of a package by name and finds the first version that satisfies the version range of the dependency
let getSatisfyingPackageVersion (nodeManager: NodeManager) (pkg : NpmDependency) =
    let packageVersions =
        match nodeManager with
        | NodeManager.Npm ->
            let res =
                CreateProcess.xplatCommand "npm" [ "show"; pkg.Name; "versions"; "--json" ]
                |> CreateProcess.redirectOutput
                |> CreateProcess.ensureExitCode
                |> Proc.run

            Decode.unsafeFromString (Decode.list Decode.string) res.Result.Output

        | NodeManager.Yarn ->
            let res =
                CreateProcess.xplatCommand "yarn" [ "info"; pkg.Name; "versions"; "--json" ]
                |> CreateProcess.redirectOutput
                |> CreateProcess.ensureExitCode
                |> Proc.run

            Decode.unsafeFromString (Decode.field "data" (Decode.list Decode.string)) res.Result.Output

    pkg.Constraint
    |> Option.bind (fun range ->
        if pkg.LowestMatching then
            packageVersions
            |> Seq.ofList
            |> range.Satisfying
            |> Seq.tryHead
        else
            packageVersions
            |> Seq.ofList
            |> range.MaxSatisfying
            |> function
                | null -> None
                | version -> Some version
    )

let private printInstallHint (nodeManager : NodeManager) (pkg : NpmDependency) =

    let satisfyingVersion = getSatisfyingPackageVersion nodeManager pkg

    match satisfyingVersion with
    | Some version ->
        let hint =
            match nodeManager with
            | NodeManager.Npm -> sprintf "%s install %s@%s" nodeManager.CommandName pkg.Name version
            | NodeManager.Yarn -> sprintf "%s add %s@%s" nodeManager.CommandName pkg.Name version

        logger.Error("  | -- Resolve this issue using '{Hint}'", hint)

    | None ->
        ()

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
                let requiredVersion = getSatisfyingPackageVersion nodeManager package
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
                            let requiredVersion = getSatisfyingPackageVersion nodeManager package
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

/// Performs dependency analysis of a single Fable project based on it's metadata and the installed npm packages
let rec analyzePackagesForLibrary
    (nodeManager : NodeManager)
    (library : LibraryWithNpmDeps)
    (packagesToVerify : NpmDependency list)
    (installedPackages : ResizeArray<InstalledNpmPackage>)
    (isOk : bool) =

    match packagesToVerify with
    | pkg::rest ->
        logger.Information("")
        let installed = installedPackages |> Seq.tryFind (fun p -> p.Name = pkg.Name)
        let result =
            match installed with
            | None ->
                logger.Error("{Library} depends on npm package {Package}", library.Name, pkg.Name, pkg.RawVersion)
                logger.Error("  | -- Required range {Range} found in project file", pkg.Constraint |> Option.map string |> Option.defaultValue pkg.RawVersion)
                logger.Error("  | -- Missing {package} in package.json", pkg.Name)
                printInstallHint nodeManager pkg
                false

            | Some installedPackage  ->
                match installedPackage.Range, installedPackage.Installed with
                | Some range, Some version ->
                    logger.Information("{Library} depends on npm package {Package}", library.Name, pkg.Name);
                    logger.Information("  | -- Required range {Range} found in project file", pkg.Constraint |> Option.map string |> Option.defaultValue pkg.RawVersion)
                    logger.Information("  | -- Used range {Range} in package.json", range.ToString())
                    match pkg.Constraint with
                    | Some requiredRange when requiredRange.IsSatisfied version ->
                        logger.Information("  | -- √ Installed version {Version} satisfies required range {Range}", version.ToString(), requiredRange.ToString())
                        true

                    | _ ->
                        logger.Error("  | -- Installed version {Version} does not satisfy required range {Range}", version.ToString(), pkg.Constraint |> Option.map string |> Option.defaultValue pkg.RawVersion)
                        printInstallHint nodeManager pkg
                        false

                | _ ->
                    logger.Error("{Library} requires npm package {Package} version {Version} which was not installed", library.Name, pkg.Name, pkg.Constraint.ToString())
                    false

        analyzePackagesForLibrary nodeManager library rest installedPackages (isOk && result)
    | [] ->
        isOk

/// Performs dependency analysis of multiple Fable projects based on their metadata and the installed npm packages
let rec analyzePackages
    (nodeManager : NodeManager)
    (libraries : LibraryWithNpmDeps list)
    (installedPackages : ResizeArray<InstalledNpmPackage>)
    (isOk : bool) =

    match libraries with
    | library::rest ->
        let result =
            analyzePackagesForLibrary nodeManager library library.NpmDependencies installedPackages true

        analyzePackages nodeManager rest installedPackages (isOk && result)
    | [] ->
        isOk

type FemtoArgs = {
    /// The project on which the analysis is performed
    Project: string option
    /// When set to true, validates the metadata of the npm dependencies of a Fable library
    PreviewMetadata: bool
    /// When set to true, performs the required actions for full package resolution
    Resolve : bool
    /// When set to true, shows the actions required for full package resolution without actually performing them
    ResolvePreview: bool
}

let defaultCliArgs = {
    Project = None
    PreviewMetadata = false
    Resolve = false
    ResolvePreview = false
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
        // distinct by the combination of version and range
        |> List.distinctBy (fun (_, _, version, range) -> version, range)
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
        // distinct by the combination of version and range
        |> List.distinctBy (fun (_, _, version, range) -> version, range)
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

        logger.Information("Installing [{Libraryies}]", String.concat ", " packagesToInstall)
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

        logger.Information("Installing dev [{Libraryies}]", String.concat ", " packagesToInstall)
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

let rec private runner (args : FemtoArgs) =

    match args.Project, args.PreviewMetadata with
    | None, _ ->
        logger.Error("Project path was not correctly provided")
        FemtoResult.ProjectFileNotFound

    | Some project, true ->
        // only preview dependencies
        logger.Information("Validating project {Project}", project)
        logger.Information("Running {Command} against the project", "dotnet restore")

        let restoreResult =
            let processOutput =
                CreateProcess.xplatCommand "dotnet" [ "restore" ]
                |> CreateProcess.withWorkingDirectory ((IO.Directory.GetParent project).FullName)
                |> CreateProcess.redirectOutput
                |> Proc.run

            if processOutput.ExitCode <> 0
            then Error processOutput.Result.Output
            else Ok ()

        match restoreResult with
        | Error error ->
            logger.Error("{Command} Failed with error {Error}", "dotnet restore", error)
            FemtoResult.ValidationFailed

        | Ok () ->

            logger.Information("Ensuring project can be analyzed")
            let crackResult =
                try
                    let projectInfo = ProjectCracker.fullCrack project
                    Ok projectInfo
                with
                | ex -> Error ex.Message

            match crackResult with
            | Error er ->
                logger.Error("Error while analyzing the project's structure and dependencies")
                FemtoResult.ValidationFailed
            | Ok _ ->
                let libraryName = Path.GetFileNameWithoutExtension project
                let npmDependencies = Npm.parseDependencies project
                if List.isEmpty npmDependencies then
                    logger.Warning("Project {Project} does not contain npm dependency metadata", libraryName)
                    FemtoResult.ProjectCrackerFailed
                else
                    for pkg in npmDependencies do
                        logger.Information("{Library} requires npm package {Package}", libraryName, pkg.Name)
                        logger.Information("  | -- Required range {Range}", pkg.RawVersion)
                        logger.Information("  | -- Resolution strategy '{Strategy}'", if pkg.LowestMatching then "Min" else "Max")
                        match getSatisfyingPackageVersion NodeManager.Npm pkg with
                        | Some version ->
                            logger.Information("  | -- √ Found version {Version} that satisfies the required range", version)
                        | None ->
                            logger.Error("  | -- Could not find a version that satisfies the required range {Range}", pkg.RawVersion)

                    FemtoResult.ValidationSucceeded

    | Some project, false ->
        logger.Information("Analyzing project {Project}", project)
        let projectInfo =
            try Ok (ProjectCracker.fullCrack project)
            with | ex ->
                Error ex.Message

        match projectInfo with
        | Error errorMessage ->
            logger.Error("Error while analyzing project structure and dependencies")
            FemtoResult.ProjectCrackerFailed
        | Ok projectInfo ->

            let libraries = findLibraryWithNpmDeps projectInfo


            match findPackageJson project with
            | None ->
                for library in libraries do
                    for pkg in library.NpmDependencies do
                        logger.Information("{Library} requires npm package {Package} ({Version})", library.Name, pkg.Name, pkg.RawVersion)

                logger.Warning "Could not locate package.json file"
                FemtoResult.MissingPackageJson

            | Some packageJson ->
                let packageJsonDir = (IO.Directory.GetParent packageJson).FullName
                let nodeManager = workspaceCommand packageJson
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

                    runner args

                else

                    let installedPackages = findInstalledPackages packageJson
                    logger.Information("Using {Manager} for package management", nodeManager.CommandName)
                    if args.ResolvePreview then
                        logger.Information("Previewing required actions for package resolution")
                        let resolveActions = autoResolve nodeManager libraries installedPackages []
                        let simplifiedActions = resolveConflicts resolveActions
                        if List.isEmpty simplifiedActions then
                            logger.Information("√ Required packages are already resolved")
                            FemtoResult.ValidationSucceeded
                        else
                        for action in simplifiedActions do
                            match action with
                            | ResolveAction.Install(lib, pkg, version, range) ->
                                logger.Information("{Library} -> Install {Package} version {Version} satisfies [{Range}]", lib, pkg, version, range)
                            | ResolveAction.InstallDev(lib, pkg, version, range) ->
                                logger.Information("{Library} -> Install {Package} (dev) version {Version} satisfies [{Range}]", lib, pkg, version, range)
                            | ResolveAction.Uninstall(lib, pkg, version) ->
                                logger.Information("{Library} -> Uninstall {Package} version {Version}", lib, pkg, version)
                            | ResolveAction.UninstallDev(lib, pkg, version) ->
                                logger.Information("{Library} -> Uninstall {Package} (dev) version {Version}", lib, pkg, version)
                            | ResolveAction.UnableToResolve(lib, pkg, range, error) ->
                                logger.Error("{Library} -> Unable to resolve version for {Package} within {Range}", lib, pkg, range)
                                logger.Error(error)

                        FemtoResult.ValidationSucceeded
                    elif args.Resolve then
                        let resolveActions = autoResolve nodeManager libraries installedPackages []
                        if List.isEmpty resolveActions then
                            logger.Information("√ Required packages are already resolved")
                            FemtoResult.ValidationSucceeded
                        else
                            logger.Information("Executing required actions for package resolution")
                            try
                                let cwd = (IO.Directory.GetParent packageJson).FullName
                                executeResolutionActions cwd nodeManager resolveActions
                                logger.Information("√ Package resolution complete")
                                FemtoResult.ValidationSucceeded
                            with
                            | ex ->
                                logger.Error(ex.Message)
                                FemtoResult.UnexpectedError
                    else
                    if analyzePackages nodeManager libraries installedPackages true then
                        logger.Information("")
                        logger.Information("√ Project analysis complete")
                        FemtoResult.ValidationSucceeded
                    else
                        FemtoResult.ValidationFailed


type CLIArguments =
    | [<MainCommand; ExactlyOnce>] Project of path : string
    | Validate
    | Resolve
    | Preview

    interface IArgParserTemplate with

        member this.Usage =
            match this with
            | Project _ -> "specify the path to the F# project"
            | Validate -> "check that the XML tags used in the F# project file are parsable and a npm package version can be calculated"
            | Resolve -> "resolve and install required packages"
            | Preview -> "preview required actions for package resolution"

let parseArgs (cliArgs : CLIArguments list) =
    let rec apply (cliArgs : CLIArguments list) (res : FemtoArgs) =
        match cliArgs with
        | Project project::rest ->
            let project =
                if Path.isRelativePath project then
                    Path.GetFullPath project
                else
                    project

            apply rest { res with Project = Some project }

        | Validate::rest ->
            apply rest { res with PreviewMetadata = true }

        | Resolve::rest ->
            apply rest { res with Resolve = true }

        | Preview::rest ->
            apply rest { res with ResolvePreview = true }

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
    let parser = ArgumentParser.Create<CLIArguments>("femto")

    let printUsage() =
        parser.PrintUsage()
        |> printfn "%s" // We don't use the logger in order to avoid the [time type] prefix in the console

    let result =
        try
            let args = parser.Parse(argv)

            if args.IsUsageRequested then
                printUsage()
                FemtoResult.UsageRequested
            else
                args.GetAllResults()
                |> parseArgs
                |> runner
        with
            | _ ->
                printUsage()
                FemtoResult.InvalidArguments

    int result