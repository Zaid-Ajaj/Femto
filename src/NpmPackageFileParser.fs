module Femto.NpmPackageFileParser

open Thoth.Json.Net
open fastJSON5
open System.Collections.Generic
open Legivel.Attributes
open System

type PackageFile = {
    [<YamlField("dependencies")>] Dependencies : Map<string, string> option
    [<YamlField("devDependencies")>] DevDependencies : Map<string, string> option
}

module Json5 =
    [<CLIMutable>]
    type Json5PackageFile = {
        Dependencies : Dictionary<string, string>
        DevDependencies : Dictionary<string, string>
    }

    let parseDependencies json5 =
        try
            let parseResult = JSON5.ToObject<Json5PackageFile>(json5)
            {
                PackageFile.Dependencies =
                    Option.ofObj parseResult.Dependencies
                    |> Option.map (Seq.map (|KeyValue|) >> Map.ofSeq)
                PackageFile.DevDependencies =
                    Option.ofObj parseResult.DevDependencies
                    |> Option.map (Seq.map (|KeyValue|) >> Map.ofSeq)
            }
            |> Ok
        with
        | e -> Error e.Message

module Yaml =
    let parseDependencies yaml =
        match Legivel.Serialization.Deserialize<PackageFile> yaml with
        | Legivel.Serialization.Success s :: _ -> Ok s.Data
        | Legivel.Serialization.Error e :: _ ->
            e.Error
            |> List.map (fun x -> x.Message)
            |> String.concat "\n"
            |> Error
        | [] -> Error "No YAML document found"

module Json =
    let parseDependencies json =
        Decode.Auto.fromString<PackageFile>(json, isCamelCase = true)

let parseDependencies packageFile =
    let file = IO.File.ReadAllText packageFile
    if packageFile.EndsWith "json5" then
        Json5.parseDependencies file
    elif packageFile.EndsWith "yaml" then
        Yaml.parseDependencies file
    else
        Json.parseDependencies file
