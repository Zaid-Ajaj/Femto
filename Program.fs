open Femto
open Femto.ProjectCracker
open System
open Serilog
open Npm
open Newtonsoft.Json.Linq
open FSharp.Compiler.AbstractIL.Internal.Library
open System.Diagnostics

let logger = LoggerConfiguration().WriteTo.Console().CreateLogger()

let findNpmDependencies (project: CrackedFsproj) =
    [ yield project.ProjectFile
      yield! project.ProjectReferences
      for package in project.PackageReferences do yield package.FsprojPath ]
    |> List.map (fun proj -> proj, Path.GetFileNameWithoutExtension proj, Npm.parseDependencies (Path.normalizeFullPath proj))
    |> List.filter (fun (path, name, deps) -> not (List.isEmpty deps))

let rec findPackageJson (project: string) =
    let parentDir = IO.Directory.GetParent project
    if isNull parentDir then None
    else
      parentDir.FullName
      |> IO.Directory.GetFiles
      |> Seq.tryFind (fun file -> file.EndsWith "package.json")
      |> Option.orElse (findPackageJson parentDir.FullName)

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
    let npmDependencies = findNpmDependencies projectInfo
    match findPackageJson project with
    | None ->
        for (path, name, packages) in npmDependencies do
            for pkg in packages do
                logger.Information("{Library} requires npm package {Package} ({Version})", name, pkg.Name, pkg.RawVersion)
        logger.Warning "Could not locate package.json file"
        0
    | Some packageJson ->
        match needsNodeModules packageJson with
        | Some command ->
            logger.Information("Npm packages need to be restored first")
            logger.Information("Restore npm packages using {Command}", command)
            0
        | None ->
        let installedPackages = findInstalledPackages packageJson
        for (path, name, packages) in npmDependencies do
            for pkg in packages do
                logger.Information("")
                let installed = installedPackages |> Seq.tryFind (fun p -> p.Name = pkg.Name)
                match installed with
                | None ->
                    logger.Error("{Library} depends on npm package '{Package}'", name, pkg.Name, pkg.RawVersion)
                    logger.Error("  | -- Required range {Range} found in project file", pkg.Constraint |> Option.map string |> Option.defaultValue pkg.RawVersion)
                    logger.Error("  | -- Missing '{package}' in package.json", pkg.Name)
                    if pkg.InstallHint <> ""
                    then logger.Error("  | -- Resolve this issue using {Hint}", pkg.InstallHint)
                | Some installedPackage  ->
                    match installedPackage.Range, installedPackage.Installed with
                    | Some range, Some version ->
                        logger.Information("{Library} depends on npm package '{Package}'", name, pkg.Name);
                        logger.Information("  | -- Required range {Range} found in project file", pkg.Constraint |> Option.map string |> Option.defaultValue pkg.RawVersion)
                        logger.Information("  | -- Used range {Range} in package.json", range.ToString())
                        match pkg.Constraint with
                        | Some requiredRange when requiredRange.IsSatisfied version ->
                            logger.Information("  | -- √ Installed version {Version} satisfies required range {Range}", version.ToString(), requiredRange.ToString())
                        | _ ->
                            logger.Error("  | -- Installed version {Version} does not satisfy required range {Range}", version.ToString(), pkg.Constraint |> Option.map string |> Option.defaultValue pkg.RawVersion)
                            if pkg.InstallHint <> ""
                            then logger.Error("  | -- Resolve this issue using {Hint}", pkg.InstallHint)

                    | _ ->
                        logger.Error("{Library} requires npm package '{Package}' ({Version}) which was not installed", name, pkg.Name, pkg.Constraint.ToString())
        0


