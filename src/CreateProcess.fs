namespace Femto

open Fake.SystemHelper
open Fake.Core

module CreateProcess =
    /// Creates a cross platform command from the given program and arguments.
    ///
    /// For example:
    ///
    /// ```fsharp
    /// CreateProcess.xplatCommand "npm" [ "install" ]
    /// ```
    ///
    /// Will be the following on windows
    ///
    /// ```fsharp
    /// CreateProcess.fromRawCommand "cmd" [ "/C"; "npm"; "install" ]
    /// ```
    /// And the following otherwise
    ///
    /// ```fsharp
    /// CreateProcess.fromRawCommand "npm" [ "install" ]
    /// ```
    let xplatCommand program args =
        let program', args' =
            if Environment.isWindows
            then "cmd", List.concat [ [ "/C"; program ]; args ]
            else program, args

        CreateProcess.fromRawCommand program' args'

    let withOptionalWorkingDirectory (dir: string option) proc = 
        match dir with 
        | Some directory -> 
            proc 
            |> CreateProcess.withWorkingDirectory directory 
        | None -> 
            proc

    let run proc = Proc.run proc