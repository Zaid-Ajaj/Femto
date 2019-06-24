open Femto
open Femto.ProjectCracker
open System
open Serilog
open Npm
open Newtonsoft.Json.Linq
open FSharp.Compiler.AbstractIL.Internal.Library
open System.Diagnostics
open Fake.Core
open Thoth.Json.Net
open Fake.SystemHelper
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
    | Install of package:string * library:string * version:string
    | Uninstall of package:string * library: string * version:string
    | UnableToResolve of package:string * library:string * range:string * error: string

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

let rec findPackageJson (project: string) =
    let parentDir = IO.Directory.GetParent project
    if isNull parentDir then None
    else
      parentDir.FullName
      |> IO.Directory.GetFiles
      |> Seq.tryFind (fun file -> file.EndsWith "package.json")
      |> Option.orElse (findPackageJson parentDir.FullName)

[<RequireQualifiedAccess>]
type NodeManager =
    | Yarn
    | Npm

    member this.CommandName =
        match this with
        | Yarn -> "yarn"
        | Npm -> "npm"

let workspaceCommand (packageJson: string) =
    let parentDir = IO.Directory.GetParent packageJson
    let siblings = [ yield! IO.Directory.GetFiles parentDir.FullName; yield! IO.Directory.GetDirectories parentDir.FullName ]
    let yarnLockExists = siblings |> List.exists (fun file -> file.EndsWith "yarn.lock")
    if yarnLockExists
    then NodeManager.Yarn
    else NodeManager.Npm

let needsNodeModules (packageJson: string) =
    let parentDir = IO.Directory.GetParent packageJson
    let siblings = [ yield! IO.Directory.GetFiles parentDir.FullName; yield! IO.Directory.GetDirectories parentDir.FullName ]
    let nodeModulesExists = siblings |> List.exists (fun file -> file.EndsWith "node_modules")
    not nodeModulesExists

let findInstalledPackages (packageJson: string) : ResizeArray<InstalledNpmPackage> =
    let parentDir = IO.Directory.GetParent packageJson
    let siblings = [ yield! IO.Directory.GetFiles parentDir.FullName; yield! IO.Directory.GetDirectories parentDir.FullName ]
    let nodeModulesExists = siblings |> List.tryFind (fun file -> file.EndsWith "node_modules")
    let yarnLockExists = siblings |> List.tryFind (fun file -> file.EndsWith "yarn.lock")
    let packageLockExists = siblings |> List.tryFind (fun file -> file.EndsWith "package-lock.json")
    let content = JObject.Parse(IO.File.ReadAllText packageJson)
    let dependencies : JProperty list = [
        if content.ContainsKey "dependencies"
        then yield! (content.["dependencies"] :?> JObject).Properties() |> List.ofSeq

        if content.ContainsKey "devDependencies"
        then yield! (content.["devDependencies"] :?> JObject).Properties() |> List.ofSeq

        if content.ContainsKey "peerDependencies"
        then yield! (content.["peerDependencies"] :?> JObject).Properties() |> List.ofSeq
    ]

    let topLevelPackages = ResizeArray [
        for package in dependencies -> {
            Name = package.Name;
            Range = Some (SemVer.Range(package.Value.ToObject<string>()));
            Installed = None
        }
    ]

    match yarnLockExists, packageLockExists, nodeModulesExists with
    | None, None, None ->
        topLevelPackages
    | Some yarnLockFile, None, Some nodeModulePath ->
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
    | None, Some packageLockFile, Some nodeModulePath ->
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
    | _ ->
        topLevelPackages

let getSatisfyingPackageVersion (nodeManager: NodeManager) (pkg : NpmDependency) =
    let packageVersions =
        match nodeManager with
        | NodeManager.Npm ->
            let program, args =
                if Environment.isWindows
                then "cmd", [ "/C"; "npm"; "show"; pkg.Name; "versions"; "--json" ]
                else "npm", [ "show"; pkg.Name; "versions"; "--json" ]

            let res =
                CreateProcess.fromRawCommand program args
                |> CreateProcess.redirectOutput
                |> CreateProcess.ensureExitCode
                |> Proc.run

            Decode.unsafeFromString (Decode.list Decode.string) res.Result.Output

        | NodeManager.Yarn ->
            let program, args =
                if Environment.isWindows
                then "cmd", [ "/C"; "yarn"; "info"; pkg.Name; "versions"; "--json" ]
                else "yarn", [ "info"; pkg.Name; "versions"; "--json" ]

            let res =
                CreateProcess.fromRawCommand program args
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
                    [ ResolveAction.Install(library.Name, package.Name, version) ]

            | Some installedPackage ->
                // already installed -> check whether it falls under the required constraint
                match installedPackage.Range, installedPackage.Installed with
                | Some range, Some installedVersion ->
                    match package.Constraint with
                    | Some requiredRange  ->
                        if requiredRange.IsSatisfied installedVersion
                        then
                            // no need to do anything
                            [ ]
                        else
                            // installed version falls outside of required range
                            // resolve version from required range
                            let requiredVersion = getSatisfyingPackageVersion nodeManager package
                            match requiredVersion with
                            | Some resolvedVersion ->
                                [
                                    // uninstall current
                                    ResolveAction.Uninstall(library.Name, installedPackage.Name, installedVersion.ToString())
                                    // resolve again
                                    ResolveAction.Install(library.Name, package.Name, resolvedVersion)
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

let rec checkPackages
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
                logger.Error("{Library} depends on npm package '{Package}'", library.Name, pkg.Name, pkg.RawVersion)
                logger.Error("  | -- Required range {Range} found in project file", pkg.Constraint |> Option.map string |> Option.defaultValue pkg.RawVersion)
                logger.Error("  | -- Missing '{package}' in package.json", pkg.Name)
                printInstallHint nodeManager pkg
                false

            | Some installedPackage  ->
                match installedPackage.Range, installedPackage.Installed with
                | Some range, Some version ->
                    logger.Information("{Library} depends on npm package '{Package}'", library.Name, pkg.Name);
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
                    logger.Error("{Library} requires npm package '{Package}' ({Version}) which was not installed", library.Name, pkg.Name, pkg.Constraint.ToString())
                    false

        checkPackages nodeManager library rest installedPackages (isOk && result)
    | [] ->
        isOk

let rec analyzePackages
    (nodeManager : NodeManager)
    (libraries : LibraryWithNpmDeps list)
    (installedPackages : ResizeArray<InstalledNpmPackage>)
    (isOk : bool) =

    match libraries with
    | library::rest ->
        let result =
            checkPackages nodeManager library library.NpmDependencies installedPackages true

        analyzePackages nodeManager rest installedPackages (isOk && result)
    | [] ->
        isOk

type FemtoArgs = {
    Project: string option
    PreviewMetadata: bool
    Resolve : bool
    ResolvePreview: bool
}

let defaultCliArgs = {
    Project = None
    PreviewMetadata = false
    Resolve = false
    ResolvePreview = false
}

let executeResolutionActions (cwd: string) (manager: NodeManager) (actions: ResolveAction list) =
    let uninstallPackages =
        actions |> List.choose (function
        | ResolveAction.Uninstall(_, pkg, _)-> Some pkg
        | _ -> None)

    let installPackages =
        actions |> List.choose (function
        | ResolveAction.Install(_, pkg, version)-> Some (pkg, version)
        | _ -> None)

    if not (List.isEmpty uninstallPackages) then
        let program, args =
            match manager with
            | NodeManager.Npm ->
                if Environment.isWindows
                then "cmd", List.concat [ ["/C"; "npm"; "uninstall" ]; uninstallPackages; [ "--save" ] ]
                else "npm", List.concat [ [ "uninstall" ]; uninstallPackages; ["--save" ]]
            | NodeManager.Yarn ->
                if Environment.isWindows
                then "cmd", List.append [ "/C"; "yarn"; "remove" ] uninstallPackages
                else "yarn", List.append [ "remove" ] uninstallPackages

        logger.Information("Uninstalling [{Libraries}]", String.concat ", " uninstallPackages)
        CreateProcess.fromRawCommand program args
        |> CreateProcess.withWorkingDirectory cwd
        |> CreateProcess.ensureExitCodeWithMessage (sprintf "Error while uninstalling [%s]" (String.concat ", " uninstallPackages))
        |> CreateProcess.redirectOutput
        |> Proc.run
        |> ignore


    if not (List.isEmpty installPackages) then
        let packagesToInstall =
            installPackages
            |> List.map (fun (package, version) -> sprintf "%s@%s" package version)

        let program, args =
            match manager with
            | NodeManager.Npm ->
                if Environment.isWindows
                then "cmd", List.concat [ ["/C"; "npm"; "install"]; packagesToInstall; ["--save"] ]
                else "npm", List.concat [ ["install" ] ; packagesToInstall; [ "--save" ] ]
            | NodeManager.Yarn ->
                if Environment.isWindows
                then "cmd", List.concat [ [ "/C"; "yarn"; "add"; ]; packagesToInstall ]
                else "yarn", List.concat [ [ "add" ]; packagesToInstall ]

        logger.Information("Installing [{Libraryies}]", packagesToInstall |> String.concat ", ")
        CreateProcess.fromRawCommand program args
        |> CreateProcess.withWorkingDirectory cwd
        |> CreateProcess.ensureExitCodeWithMessage (sprintf "Error while installing %s" (packagesToInstall |> String.concat ", "))
        |> CreateProcess.redirectOutput
        |> Proc.run
        |> ignore

    // print out resolve errors
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
        let program, args =
            if Environment.isWindows
            then "cmd", [ "/C"; "dotnet"; "restore" ]
            else "dotnet", [ "restore" ]

        let restoreResult =
            let processOutput =
                CreateProcess.fromRawCommand program args
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
                logger.Information("Found package.json in {Dir}", (IO.Directory.GetParent packageJson).FullName)
                if needsNodeModules packageJson then
                    logger.Information("Npm packages need to be restored first for project analysis")
                    let nodeManager = workspaceCommand packageJson
                    let workingDirectory = IO.Directory.GetParent packageJson
                    match nodeManager with
                    | NodeManager.Npm ->
                        logger.Information("Restoring npm packages using 'npm install' inside {Dir}", (IO.Directory.GetParent packageJson).FullName)
                        let program, npmArgs =
                            if Environment.isWindows
                            then "cmd", [ "/C"; "npm"; "install" ]
                            else "npm", [ "install" ]

                        CreateProcess.fromRawCommand program npmArgs
                        |> CreateProcess.withWorkingDirectory workingDirectory.FullName
                        |> CreateProcess.redirectOutput
                        |> CreateProcess.ensureExitCode
                        |> Proc.run
                        |> ignore

                        runner args

                    | NodeManager.Yarn ->
                        logger.Information("Restoring npm packages using 'yarn install' inside {Dir}", (IO.Directory.GetParent packageJson).FullName)
                        let program, yarnArgs =
                            if Environment.isWindows
                            then "cmd", [ "/C"; "yarn"; "install" ]
                            else "yarn", [ "install" ]

                        CreateProcess.fromRawCommand program yarnArgs
                        |> CreateProcess.withWorkingDirectory workingDirectory.FullName
                        |> CreateProcess.redirectOutput
                        |> CreateProcess.ensureExitCode
                        |> Proc.run
                        |> ignore

                        runner args

                else

                    let installedPackages = findInstalledPackages packageJson
                    let nodeManager = workspaceCommand packageJson
                    logger.Information("Using {Manager} for package management", nodeManager.CommandName)
                    if args.ResolvePreview then
                        logger.Information("Previewing required actions for package resolution")
                        let resolveActions = autoResolve nodeManager libraries installedPackages []
                        if List.isEmpty resolveActions then
                            logger.Information("√ Required packages are already resolved")
                            FemtoResult.ValidationSucceeded
                        else
                        for action in resolveActions do
                            match action with
                            | ResolveAction.Install(lib, pkg, version) ->
                                logger.Information("{Library} -> Install {Package} @ {Version}", lib, pkg, version)
                            | ResolveAction.Uninstall(lib, pkg, version) ->
                                logger.Information("{Library} -> Uninstall {Package} @ {Version}", lib, pkg, version)
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
    // By default, we try to resolve the fsproj in the currect directory
    // This can be override if the user pass the project pass
    let defaultArgs =
        match siblings |> Seq.tryFind (fun f -> f.EndsWith ".fsproj") with
        | Some file ->
            { defaultCliArgs with Project = Some file }

        | None ->
            { defaultCliArgs with Project = None }

    apply cliArgs defaultArgs

[<EntryPoint>]
let rec main argv =
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
