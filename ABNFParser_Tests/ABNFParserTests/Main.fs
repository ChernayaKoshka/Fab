module ABNFParserTests

open Expecto
open System

[<EntryPoint>]
let main argv =
    let res = Tests.runTestsInAssembly defaultConfig argv
    Console.ReadLine()
    |> ignore
    res
