module NpmRegistry

open System
open System.Net.Http
open Thoth.Json.Net
open Newtonsoft.Json.Linq

type Versions =
    {
        LatestVersion : string option
        Versions : string list
    }


let private extractVersions =
    Decode.field "versions" (
        Decode.keyValuePairs (Decode.field "version" Decode.string)
        |> Decode.map (List.map snd)
    )


let fetchVersions (packageName : string) =
    async {
        let url = sprintf "https://registry.npmjs.org/%s" packageName

        use client = new HttpClient()
        use request = new HttpRequestMessage()

        request.RequestUri <- new Uri(url)
        request.Headers.Accept.Add(new Headers.MediaTypeWithQualityHeaderValue("application/vnd.npm.install-v1+json") )

        use! response = client.SendAsync(request) |> Async.AwaitTask
        response.EnsureSuccessStatusCode () |> ignore

        let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask

        return Decode.unsafeFromString extractVersions content
    }
