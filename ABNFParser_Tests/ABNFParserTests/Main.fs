module ABNFParserTests

open Expecto
open System

[<EntryPoint>]
let main argv =
    let res = Tests.runTestsInAssembly defaultConfig argv
    if res <> 0 then
        Console.ReadLine()
        |> ignore
    res
