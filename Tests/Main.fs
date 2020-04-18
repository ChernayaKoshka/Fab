open Expecto
open System
open Tests

[<EntryPoint>]
let main argv =
    let res = runTestsInAssembly defaultConfig argv
    // let res = runTests { defaultConfig with runInParallel = false } test2
    if res <> 0 then
        Console.ReadLine()
        |> ignore
    res
