module Program

open System
open System.IO
open System.Text
open Fake.IO
open Fake.Core

let path xs = Path.Combine(Array.ofList xs)

let solutionRoot = Files.findParent __SOURCE_DIRECTORY__ "Femto.sln";

let src = path [ solutionRoot; "src" ]
let tests = path [ solutionRoot; "tests" ]

let pack() =
    Shell.deleteDir (path [ "src"; "bin" ])
    Shell.deleteDir (path [ "src"; "obj" ])
    if Shell.Exec(Tools.dotnet, "pack --configuration Release", src) <> 0 then
        failwith "Pack failed"
    else
        let outputPath = path [ src; "bin"; "Release" ]
        if Shell.Exec(Tools.dotnet, sprintf "tool install -g femto --add-source %s" outputPath) <> 0
        then failwith "Local install failed"

let publish() =
    if Shell.Exec(Tools.dotnet, "pack --configuration Release", src) <> 0 then
        failwith "Pack failed"
    else
        let nugetKey =
            match Environment.environVarOrNone "NUGET_KEY" with
            | Some nugetKey -> nugetKey
            | None -> failwith "The Nuget API key must be set in a NUGET_KEY environmental variable"

        let nugetPath =
            Directory.GetFiles(path [ src; "bin"; "Release" ])
            |> Seq.head
            |> Path.GetFullPath

        if Shell.Exec(Tools.dotnet, sprintf "nuget push %s -s nuget.org -k %s" nugetPath nugetKey, src) <> 0
        then failwith "Publish failed"

let build() =
    if Shell.Exec(Tools.dotnet, "build --configuration Release", solutionRoot) <> 0
    then failwith "build failed"

[<EntryPoint>]
let main (args: string[]) =
    Console.InputEncoding <- Encoding.UTF8
    Console.OutputEncoding <- Encoding.UTF8
    Console.WriteLine(Swag.logo)
    try
        match args with
        | [| |]
        | [| "build"   |] -> build()
        | [| "pack"    |] -> pack()
        | [| "publish" |] -> publish()
        | _ -> printfn "Unknown args %A" args
        0
    with ex ->
        printfn "%A" ex
        1