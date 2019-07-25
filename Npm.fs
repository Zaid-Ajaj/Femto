module Femto.Npm

open Serilog
open System.IO
open System.Xml
open SemVer

let logger = LoggerConfiguration().WriteTo.Console().CreateLogger()

type NpmDependency = {
    Name: string
    Constraint: Range option
    RawVersion : string
    LowestMatching : bool
    DevDependency : bool
    Versions : string list
    IsFetchOk : bool
}

type InstalledNpmPackage = {
    mutable Name : string
    mutable Range : Range option
    mutable Installed : Version option
    mutable DevDependency : bool
}

let (|StartsWith|_|) (value:string) (input: string) =
    if input.StartsWith value
    then Some (input.Substring(value.Length))
    else None

let parseConstraint (input: string) =
    try Some(Range(input))
    with | ex -> None

let elementsByTag (el: XmlElement) tag =
    let elements = el.GetElementsByTagName tag
    [ for i in 0 .. elements.Count - 1 -> elements.Item i ]

let tryAttr (name: string) (node: XmlNode) =
    [ for i in 0 .. node.Attributes.Count - 1 -> node.Attributes.Item(i) :?> XmlAttribute ]
    |> List.tryFind (fun attr -> attr.Name = name)
    |> Option.map (fun attr -> attr.Value)

let attr (name: string) (node: XmlNode)  =
    tryAttr name node
    |> Option.defaultValue ""

let private parseResolutionStrategy (node : XmlNode) =
    tryAttr "ResolutionStrategy" node
    |> Option.map (fun value ->
        match value.ToLower() with
        | "min" -> true
        | "max" -> false
        | _ ->
            logger.Warning("Invalid value for 'ResolutionStrategy' attribute, accepted values are 'min' or 'max'. Using 'min' as default")
            true
    )
    |> Option.defaultValue true

let private parseDevDependency (node : XmlNode) =
    node
    |> tryAttr "DevDependency"
    |> Option.map (fun value ->
        match value.ToLower().Trim() with
        | "true" -> true
        | "false" -> false
        | _ ->
            logger.Warning("Invalid value for 'DevDependency' attribute, accepted values are 'true' or 'false'. Using 'false' as default")
            false
    )
    |> Option.defaultValue false

let preprocessVersion (input: string)  =
    input
      .ToLower()
      .Replace("&gt;", ">")
      .Replace("&lt;", "<")
      .Replace("gte", ">=")
      .Replace("gt", ">")
      .Replace("lt", "<")
      .Replace("lte", "<=")

let parseDependencies (project: string) =
    try
        let doc = XmlDocument()
        doc.LoadXml (File.ReadAllText project)
        let npmDependencies = elementsByTag doc.DocumentElement "NpmDependencies"
        match npmDependencies with
        | [ rootNode ] ->
            let npmPackages =
                "NpmPackage"
                |> elementsByTag (rootNode :?> XmlElement)
                |> List.map (fun node -> {
                    Name = attr "Name" node;
                    RawVersion = preprocessVersion (attr "Version" node)
                    Constraint = parseConstraint (preprocessVersion (attr "Version" node))
                    LowestMatching = parseResolutionStrategy node
                    DevDependency = parseDevDependency node
                    Versions = [ ]
                    IsFetchOk = true
                })

            npmPackages
        | _ ->
            []
    with
        | ex ->
            logger.Error("An error occured when parsing for dependencies")
            logger.Error(ex.Message)
            []
