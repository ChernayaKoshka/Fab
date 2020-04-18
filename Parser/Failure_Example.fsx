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
    name-part        = *(personal-part SP) last-name [SP suffix] CRLF
    name-part        =/ personal-part CRLF

    personal-part    = first-name / (initial ".")
    first-name       = *ALPHA
    initial          = ALPHA
    last-name        = *ALPHA
    suffix           = ("Jr." / "Sr." / 1*("I" / "V" / "X"))
    """.Trim()
let rules =
    testStr
    |> parseAllRules
    |> unwrap
let startDefintion = (findRule rules "name-part").Definition

let input = "Someone Old Sr.\r\n"
let (executionResult, remaining) = matchElements rules { Text = input; Pos = 0 } startDefintion