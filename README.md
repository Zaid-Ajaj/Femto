# Femto

Femto is an experimental CLI tool that verifies the compatibility of npm packages used by [Fable]() bindings.

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
[01:36:34 ERR]   | -- Resolve this issue using npm install date-fns@1.30
```

### Library Authors

In order for Femto to pick up the npm packages that your library depends upon, you must add a section in the project file of your library:
```xml
<PropertyGroup>
  <NpmDependencies>
      <NpmPackage Name="date-fns" Version=">= 1.30.0" InstallHint="npm install date-fns@1.30" />
  </NpmDependencies>
</PropertyGroup>
```
Notice here in the example, we have one npm package we depend upon which has requires a version that satisfies that range `>= 1.30.0`. If the user doesn't have that version installed or has an old version, a message will appear telling them how to solve the issue by running the command given in the `InstallHint`