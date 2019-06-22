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

let logger = LoggerConfiguration().WriteTo.Console().CreateLogger()

type LibraryWithNpmDeps = {
    Path : string
    Name : string
    NpmDependencies : NpmDependency list
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
    let nodeModulesExists = siblings |> List.exists (fun file -> file.EndsWith "node_modules")
    let yarnLockExists = siblings |> List.exists (fun file -> file.EndsWith "yarn.lock")
    if nodeModulesExists then NodeManager.Npm
    elif yarnLockExists then NodeManager.Yarn
    else NodeManager.Npm

let needsNodeModules (packageJson: string) =
    let parentDir = IO.Directory.GetParent packageJson
    let siblings = [ yield! IO.Directory.GetFiles parentDir.FullName; yield! IO.Directory.GetDirectories parentDir.FullName ]
    let nodeModulesExists = siblings |> List.exists (fun file -> file.EndsWith "node_modules")
    let yarnLockExists = siblings |> List.exists (fun file -> file.EndsWith "yarn.lock")
    if nodeModulesExists then None
    elif yarnLockExists then Some "yarn install"
    else Some "npm install"

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
}

let (|FullPath|_|) project =
    if Path.isRelativePath project
    then Some (Path.GetFullPath project)
    else Some project

let (|BoolArg|_|) (input: string) =
    match input.ToLower() with
    | "true" -> Some true
    | "false" -> Some false
    | _ -> None

let rec parseComplexArgs defaultArgs = function
    | "--project" :: FullPath project :: rest ->
        let modifiedArgs = { defaultArgs with Project = Some project }
        parseComplexArgs modifiedArgs rest

    | "--preview-metadata" :: BoolArg value :: rest ->
        let modifiedArgs = { defaultArgs with PreviewMetadata = value }
        parseComplexArgs modifiedArgs rest

    | "--preview-metadata" :: rest ->
        let modifiedArgs = { defaultArgs with PreviewMetadata = true }
        parseComplexArgs modifiedArgs rest

    | FullPath project :: rest ->
        let modifiedArgs = {defaultArgs with Project = Some project }
        parseComplexArgs modifiedArgs rest

    | _ ->
        defaultArgs

let defaultCliArgs = {
    Project = None
    PreviewMetadata = false
}

let parseArgs = function
    | [ ] ->
        let cwd = Environment.CurrentDirectory
        let siblings = IO.Directory.GetFiles cwd
        match siblings |> Seq.tryFind (fun f -> f.EndsWith ".fsproj") with
        | Some file -> { defaultCliArgs with Project = Some file;  }
        | None -> defaultCliArgs

    | [ "--preview-metadata" ] ->
        let cwd = Environment.CurrentDirectory
        let siblings = IO.Directory.GetFiles cwd
        match siblings |> Seq.tryFind (fun f -> f.EndsWith ".fsproj") with
        | Some file -> { defaultCliArgs with Project = Some file; PreviewMetadata = true }
        | None -> { defaultCliArgs with Project = None; PreviewMetadata = true }

    | complexArgs ->
        parseComplexArgs defaultCliArgs complexArgs

[<EntryPoint>]
let rec main argv =
    let args = parseArgs (List.ofArray argv)

    match args.Project, args.PreviewMetadata with
    | None, _ ->
        logger.Error("Project path was not correctly provided")
        int FemtoResult.ProjectFileNotFound

    | Some project, true ->
        // only preview dependencies
        logger.Information("Previewing Npm dependencies for {Project}", project)
        let libraryName = Path.GetFileNameWithoutExtension project
        let npmDependencies = Npm.parseDependencies project
        if List.isEmpty npmDependencies
        then
            logger.Warning("Project {Project} does not contain npm dependency metadata", libraryName)
            int FemtoResult.ValidationSucceeded
        else
        for pkg in npmDependencies do
            logger.Information("{Library} requires npm package {Package}", libraryName, pkg.Name)
            logger.Information("  | -- Required range {Range}", pkg.RawVersion)
            logger.Information("  | -- Resolution strategy '{Strategy}'", if pkg.LowestMatching then "Min" else "Max")
            match getSatisfyingPackageVersion NodeManager.Npm pkg with
            | Some version -> logger.Information("  | -- Version {Version} satisfies required range", version)
            | None -> logger.Error("  | -- Could not find a version that satisfies the required range {Range}", pkg.RawVersion)

        int FemtoResult.ValidationSucceeded

    | Some project, false ->
        logger.Information("Analyzing project {Project}", project)
        let projectInfo = ProjectCracker.fullCrack project
        let libraries = findLibraryWithNpmDeps projectInfo

        let result =
            match findPackageJson project with
            | None ->
                for library in libraries do
                    for pkg in library.NpmDependencies do
                        logger.Information("{Library} requires npm package {Package} ({Version})", library.Name, pkg.Name, pkg.RawVersion)
                logger.Warning "Could not locate package.json file"

                FemtoResult.MissingPackageJson

            | Some packageJson ->
                logger.Information("Found package.json in {Dir}", (IO.Directory.GetParent packageJson).FullName)
                match needsNodeModules packageJson with
                | Some command ->
                    logger.Information("Npm packages need to be restored first for project analysis")
                    let nodeManager = workspaceCommand packageJson
                    let workingDirectory = IO.Directory.GetParent packageJson
                    match nodeManager with
                    | NodeManager.Npm ->
                        logger.Information("Restoring npm packages using 'npm install' inside {Dir}", (IO.Directory.GetParent packageJson).FullName)
                        let program, args =
                            if Environment.isWindows
                            then "cmd", [ "/C"; "npm"; "install" ]
                            else "npm", [ "install" ]

                        CreateProcess.fromRawCommand program args
                        |> CreateProcess.withWorkingDirectory workingDirectory.FullName
                        |> CreateProcess.redirectOutput
                        |> CreateProcess.ensureExitCode
                        |> Proc.run
                        |> ignore

                        FemtoResult.fromCode (main argv)

                    | NodeManager.Yarn ->
                        logger.Information("Restoring npm packages using 'yarn install' inside {Dir}", (IO.Directory.GetParent packageJson).FullName)
                        let program, args =
                            if Environment.isWindows
                            then "cmd", [ "/C"; "yarn"; "install" ]
                            else "yarn", [ "install" ]

                        CreateProcess.fromRawCommand program args
                        |> CreateProcess.withWorkingDirectory workingDirectory.FullName
                        |> CreateProcess.redirectOutput
                        |> CreateProcess.ensureExitCode
                        |> Proc.run
                        |> ignore

                        FemtoResult.fromCode (main argv)

                | None ->
                    let installedPackages = findInstalledPackages packageJson
                    let nodeManager = workspaceCommand packageJson

                    if analyzePackages nodeManager libraries installedPackages true then
                        FemtoResult.ValidationSucceeded
                    else
                        FemtoResult.ValidationFailed

        int result
