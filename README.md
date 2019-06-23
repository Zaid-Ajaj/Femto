# Femto  [![Nuget](https://img.shields.io/nuget/v/Femto.svg?colorB=green)](https://www.nuget.org/packages/Femto)

Femto is an experimental CLI tool that verifies the compatibility of npm packages used by [Fable](https://github.com/fable-compiler/Fable) bindings.

### Install
```
dotnet tool install femto --global
```

### Usage
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
c:\>femto ./App.fsproj
[01:36:31 INF] Analyzing project c:\App.fsproj
[01:36:34 INF]
[01:36:34 INF] Fable.DateFunctions depends on npm package 'date-fns'
[01:36:34 INF]   | -- Required range >= 1.30.0 found in project file
[01:36:34 INF]   | -- Used range ^1.28.2 in package.json
[01:36:34 ERR]   | -- Installed version 1.29.0 does not satisfy required range >= 1.30.0
[01:36:34 ERR]   | -- Resolve this issue using 'npm install date-fns@1.31.0'
```

### Automatic Package resolution with `--resolve`
Femto can automagically resolve required package issues using the command `--resolve`:
```
femto --resolve

femto --resolve --project ./src/Client.fsproj
```

### Preview resolution actions with `--resolve-preview`
You can see what Femto will attempt to do without actually executing the commands themselves using `--resolve-preview`
```
femto --resolve-preview

femto --resolve-preview --project ./src/Client.fsproj
```

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

You can customize the resolution strategy by adding `ResolutionStrategy` attribute to an `NpmPackage` node. Accepted values are `min` and `max`. If `ResolutionStrategy` is not set, we default to `min` strategy.

```xml
<PropertyGroup>
  <NpmDependencies>
      <NpmPackage Name="date-fns" Version=">= 1.30.0" ResolutionStrategy="max" />
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

### Validate your dependencies
If you are a library author and wondering whether Femto will pick uo the dependencies you specified in your project file, then simply run:
```
femto --validate

femto --validate --project ./path/to/Library.fsproj
```
In the directory where you have the project file of your library. This command will check whether the library has valid metadata about the required npm packages and will resolve the versions based on the specified `ResolutionStrategy`.