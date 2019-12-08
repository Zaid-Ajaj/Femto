# Femto  [![Nuget](https://img.shields.io/nuget/v/Femto.svg?colorB=green)](https://www.nuget.org/packages/Femto)

Femto is CLI tool that manages the npm packages used by [Fable](https://github.com/fable-compiler/Fable) bindings.

Read [Introducing Femto](https://fable.io/blog/Introducing-Femto.html) for an in-depth understanding of why Femto is needed and what problem it solves. 

### Install
```
dotnet tool install femto --global
```

### Project Dependency Analysis
Simply `cd` your way to the directory where you have your Fable application and run
```bash
femto
```
Alternatively, you can specify a project file by yourself:
```
femto ./Client.fsproj
```
Here is an example output:
```bash
c:\projects\elmish-getting-started\src> femto
[18:17:09 INF] Analyzing project c:/projects/elmish-getting-started/src/App.fsproj
[18:17:11 INF] Found package.json in c:\projects\elmish-getting-started
[18:17:11 INF] Using npm for package management
[18:17:14 INF] Elmish.AnimatedTree requires npm package react-spring
[18:17:14 INF]   | -- Required range >= 8.0.0 found in project file
[18:17:14 INF]   | -- Used range ^8.0.1 in package.json
[18:17:14 INF]   | -- Found installed version 8.0.1
[18:17:14 INF]   | -- react-spring was installed into "devDependencies" instead of "dependencies"
[18:17:14 INF]   | -- Re-install as a production dependency
[18:17:14 INF]   | -- Resolve manually using 'npm uninstall react-spring' then 'npm install react-spring@8.0.1 --save'
[18:17:14 INF] Fable.DateFunctions requires npm package date-fns
[18:17:14 INF]   | -- Required range >= 1.30 < 2.0 found in project file
[18:17:14 INF]   | -- Missing date-fns in package.json
[18:17:14 INF]   | -- Resolve manually using 'npm install date-fns@1.30.1 --save'
[18:17:14 INF] Elmish.SweetAlert requires npm package sweetalert2
[18:17:14 INF]   | -- Required range >= 8.5 found in project file
[18:17:14 INF]   | -- Missing sweetalert2 in package.json
[18:17:14 INF]   | -- Resolve manually using 'npm install sweetalert2@8.5.0 --save'
```

### Automatic Package resolution with `--resolve`
Femto can automagically resolve required package issues using the command `--resolve`:
```
femto --resolve

femto --resolve ./src/Client.fsproj
```
This command checks for missing packages and packages of which the installed version does not satisfy the required version found in npm dependency metadata of the used Fable packages.
 - If a package is missing then it is installed.
 - If a package version doesn't satisfy requirements, then a proper version is resolved and the package is replaced with the new resolved version by uninstalling the current one and installing the correct package.
 - If a package version doesn't satisfy requirements *and* a version cannot be resolved that satisfies requirements, then a resolution error is logged.

### Installing Packages with `femto install <package>`
Femto can install a package for a project whether it is using paket or not then automatically resolves the required npm packages afterwards. Simply navigate to a project where there is a fsharp/Fable project and call
```
femto install <package>
```
First, Femto detects whether it needs to use paket by checking the existence of a `paket.references` file, if that is the case then Femto also detects in which dependency group the package has to be installed and eventually calls the installed paket instance to install the package. Afterwards, Femto calls `--resolve` to install potentially required npm packages. When there is no `paket.references` file, then Femto simply calls `dotnet add package <package>` and then `femto --resolve` in the project directory.

> When Femto cannot find paket installed as a global dotnet tool nor as a local installation from the paket bootstrapper under `.paket/paket.exe`, then Femto will install it locally following these [Paket gettings started guidelines](https://fsprojects.github.io/Paket/getting-started.html) and restarts the installation process.

### Uninstalling Packages with `femto uninstall <package>`

Same as when installing a package, Femto will instruct either nuget or paket to uninstall the package except this command does not remove unused packages from npm in package.json because Femto cannot know whether the package is being used multiple projects.

### Library Authors

In order for Femto to pick up the npm packages that your library depends upon, you must add a section in the project file of your library:
```xml
<PropertyGroup>
  <NpmDependencies>
      <NpmPackage Name="date-fns" Version=">= 1.30.0" />
  </NpmDependencies>
</PropertyGroup>
```
Notice here in the example, we have one npm package we depend upon which has requires a version that satisfies that range `>= 1.30.0`. If the user doesn't have that version installed or has an old version, a message will appear telling them how to solve the issue.

### Resolution Strategy

You can customize the resolution strategy by adding `ResolutionStrategy` attribute to an `NpmPackage` node. Accepted values are `min` and `max` (case-insensitive). If `ResolutionStrategy` is not set, we default to `min` strategy.

```xml
<PropertyGroup>
  <NpmDependencies>
      <NpmPackage Name="date-fns" Version=">= 1.30.0" ResolutionStrategy="max" />
  </NpmDependencies>
</PropertyGroup>
```

### Development Dependency

You can specify whether the npm package your library depends upon is actually a development dependency instead a production dependency using the `DevDependency` attribute. Package resolution take development dependencies into account and if the developer had installed the package by mistake in production dependencies then the automatic resolver will un-install it and re-install it a development dependency.

```xml
<PropertyGroup>
  <NpmDependencies>
      <NpmPackage Name="date-fns" Version=">= 1.30.0" DevDependency="true" />
  </NpmDependencies>
</PropertyGroup>
```

### Unsupported Version Ranges

Sometimes you want to restrict the version using a specific range such as `>= 1.0 < 2` This range cannot be set inside the attribute value of `Version` because the XML would be invalid and you would not be able to `dotnet restore` the project. In these cases you can replace the operator with it's abbreviated name:
 - `>=` becomes `gte`
 - `>` become `gt`
 - `<=` becomes `lte`
 - `<` becomes `lt`

This way you can specify your version range as `gte 1.0 lt 2` or you can mix-and-match the notations `>= 1.0 lt 2`

### Visual Studio Users

If you happen to use Visual Studio to build your library, it will escape symbols like `>=` or `>` from the project and turn them into `&gt;=` or `&gt;`. Don't worry about this because Femto can still understand the underlying symbols. To make sure everything still works, run `femto --validate` (see below) against your project and you should see the same results.


### Validate your dependencies
If you are a library author and wondering whether Femto will pick uo the dependencies you specified in your project file, then simply run:
```
femto --validate

femto --validate ./path/to/Library.fsproj
```
In the directory where you have the project file of your library. This command will check whether the library has valid metadata about the required npm packages and will try to resolve the versions based on the specified `ResolutionStrategy`.
