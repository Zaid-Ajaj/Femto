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
}

type InstalledNpmPackage = {
    mutable Name : string
    mutable Range : Range option
    mutable Installed : Version option
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
                        RawVersion = attr "Version" node
                        Constraint = parseConstraint (attr "Version" node)
                        LowestMatching = parseResolutionStrategy node
                    })

            npmPackages
        | _ ->
            []
    with
        | ex ->
            logger.Error("An error occured when parsing for dependencies")
            logger.Error(ex.Message)
            []
