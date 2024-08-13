module Femto.Json5Parser

open fastJSON5
open System.Collections.Generic

[<CLIMutable>]
type PackageFile = {
    Dependencies : Dictionary<string, string>
    DevDependencies : Dictionary<string, string>
}

let parsePackageJson5 json5 =
    try
        let parseResult = JSON5.ToObject<PackageFile>(json5)
        {|
            Dependencies = Option.ofObj parseResult.Dependencies |> Option.map (Seq.map (|KeyValue|) >> Map.ofSeq)
            DevDependencies = Option.ofObj parseResult.DevDependencies |> Option.map (Seq.map (|KeyValue|) >> Map.ofSeq)
        |}
        |> Ok
    with
    | e -> Error e.Message
