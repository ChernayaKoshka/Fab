#r @".\bin\Debug\netcoreapp3.1\FParsecCS.dll"
#r @".\bin\Debug\netcoreapp3.1\FParsec.dll"
#r @".\bin\Debug\netcoreapp3.1\Parser.dll"
open ABNF
open ABNF.Execution
open FParsec

let unwrap parseResult =
    match parseResult with
    | Success(res, _, _) ->
        res
    | Failure (err, _, _) ->
        failwithf "%A" err

let testStr =
    """
    test = *(ALPHA) ALPHA
    """.Trim()
let rules =
    testStr
    |> parseAllRules
    |> unwrap
let startDefintion = (findRule rules "test").Definition

let input = "ABC"
let (executionResult, remaining) = matchElements rules { Text = input; Pos = 0 } startDefintion