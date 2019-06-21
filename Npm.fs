module Femto.Npm

open System.IO
open System.Xml
open SemVer

type NpmDependency = {
    Name: string
    Constraint: Range option
    RawVersion : string
    InstallHint: string
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

let attr (name: string) (node: XmlNode)  =
    [ for i in 0 .. node.Attributes.Count - 1 -> node.Attributes.Item(i) :?> XmlAttribute ]
    |> List.tryFind (fun attr -> attr.Name = name)
    |> Option.map (fun attr -> attr.Value)
    |> Option.defaultValue ""

let parseDependencies (project: string) =
    try
        let doc = XmlDocument()
        doc.LoadXml (File.ReadAllText project)
        let npmDependencies = elementsByTag doc.DocumentElement "NpmDependencies"
        match npmDependencies with
        | [ rootNode ] ->
            "NpmPackage"
            |> elementsByTag (rootNode :?> XmlElement)
            |> List.map (fun node -> {
                   Name = attr "Name" node;
                   RawVersion = attr "Version" node
                   Constraint = parseConstraint (attr "Version" node)
                   InstallHint = attr "InstallHint" node
                })
        | _ ->
            []
    with
    | ex -> []
