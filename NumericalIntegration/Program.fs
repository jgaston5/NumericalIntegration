﻿// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open RiemanSum

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    let item = RHS
    printfn "Hello world %s" message
    0 // return an integer exit code