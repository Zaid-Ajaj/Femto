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

let logger = LoggerConfiguration().WriteTo.Console().CreateLogger()

type LibraryWithNpmDeps = {
    Path : string
    Name : string
    NpmDependencies : NpmDependency list
    LowestMatching : bool
}

let findLibraryWithNpmDeps (project: CrackedFsproj) =
    [ yield project.ProjectFile
      yield! project.ProjectReferences
      for package in project.PackageReferences do yield package.FsprojPath ]
    |> List.map (fun proj ->
        let (lowestMatching, npmDeps) = Npm.parseDependencies (Path.normalizeFullPath proj)
        {
            Path = proj
            Name = Path.GetFileNameWithoutExtension proj
            LowestMatching = lowestMatching
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
    let packageLockExists = siblings |> List.exists (fun file -> file.EndsWith "package-lock.json")
    if nodeModulesExists then NodeManager.Npm
    elif yarnLockExists then NodeManager.Yarn
    else NodeManager.Npm

let needsNodeModules (packageJson: string) =
    let parentDir = IO.Directory.GetParent packageJson
    let siblings = [ yield! IO.Directory.GetFiles parentDir.FullName; yield! IO.Directory.GetDirectories parentDir.FullName ]
    let nodeModulesExists = siblings |> List.exists (fun file -> file.EndsWith "node_modules")
    let yarnLockExists = siblings |> List.exists (fun file -> file.EndsWith "yarn.lock")
    let packageLockExists = siblings |> List.exists (fun file -> file.EndsWith "package-lock.json")
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

let private printInstallHint (nodeManager : NodeManager) (library : LibraryWithNpmDeps) (pkg : NpmDependency) =

    let packageVersions =
        match nodeManager with
        | NodeManager.Npm ->
            let res =
                CreateProcess.fromRawCommand "npm" [ "show"; pkg.Name; "versions"; "--json" ]
                |> CreateProcess.redirectOutput
                |> CreateProcess.ensureExitCode
                |> Proc.run

            Decode.unsafeFromString (Decode.list Decode.string) res.Result.Output

        | NodeManager.Yarn ->
            let res =
                CreateProcess.fromRawCommand "yarn" [ "info"; pkg.Name; "versions"; "--json" ]
                |> CreateProcess.redirectOutput
                |> CreateProcess.ensureExitCode
                |> Proc.run

            Decode.unsafeFromString (Decode.field "data" (Decode.list Decode.string)) res.Result.Output

    let maxSatisfyingVersion =
        pkg.Constraint
        |> Option.map (fun range ->
            let lowestMatching =
                // Can we resolve this using Boolean algebra?
                match pkg.LowestMatching with
                | Some pkgLowestMatching ->
                    if library.LowestMatching <> pkgLowestMatching then
                        logger.Information("  | -- Resolution strategy override to lowest matching: {Strategy}", pkg.LowestMatching)
                        pkgLowestMatching
                    else
                        library.LowestMatching
                | None ->
                    library.LowestMatching

            if lowestMatching then
                packageVersions
                |> Seq.cast<string>
                |> range.Satisfying
                |> Seq.head
            else
                packageVersions
                |> Seq.cast<string>
                |> range.MaxSatisfying
        )

    match maxSatisfyingVersion with
    | Some maxSatisfyingVersion ->
        let hint =
            sprintf "%s install %s@%s" nodeManager.CommandName pkg.Name maxSatisfyingVersion

        logger.Error("  | -- Resolve this issue using '{Hint}'", hint)
    | None -> ()

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
                printInstallHint nodeManager library pkg
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
                        printInstallHint nodeManager library pkg
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
        logger.Information("Resolution strategy for {Library} is lowest matching: {LowestMatching}", library.Name, library.LowestMatching)

        let result =
            checkPackages nodeManager library library.NpmDependencies installedPackages true

        analyzePackages nodeManager rest installedPackages (isOk && result)
    | [] ->
        isOk

[<EntryPoint>]
let main argv =
    let project =
        match argv with
        | [| |]  ->
            let cwd = Environment.CurrentDirectory
            let siblings = IO.Directory.GetFiles cwd
            match siblings |> Seq.tryFind (fun f -> f.EndsWith ".fsproj") with
            | Some file -> file
            | None -> failwith "This directory does not contain any F# projects"
        | args ->
            if Path.isRelativePath args.[0]
            then Path.GetFullPath args.[0]
            else args.[0]

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
            match needsNodeModules packageJson with
            | Some command ->
                logger.Information("Npm packages need to be restored first")
                logger.Information("Restore npm packages using {Command}", command)

                FemtoResult.NodeModulesNotInstalled

            | None ->
                let installedPackages = findInstalledPackages packageJson
                let nodeManager = workspaceCommand packageJson

                if analyzePackages nodeManager libraries installedPackages true then
                    FemtoResult.ValidationSucceeded
                else
                    FemtoResult.ValidationFailed

    int result
