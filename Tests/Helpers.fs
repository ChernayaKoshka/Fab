[<RequireQualifiedAccess>]
module Helpers

open System
open System.Text.RegularExpressions
open FParsec
open Expecto

let unwrap parseResult =
    match parseResult with
    | Success(res, _, _) ->
        res
    | Failure (err, _, _) ->
        failwithf "%A" err

let run parser =
    runParserOnString parser [ ] ""

open Generator

let generateSingle ( res : ParserResult<Rule, RuleName list> ) =
    res
    |> unwrap
    |> List.singleton
    |> generate
    |> Map.toSeq
    |> Seq.exactlyOne
    |> snd

let expectWellFormedRegex (str : string) =
    let result =
        if String.IsNullOrWhiteSpace str then
            false
        else
            let regex = Regex(str)
            try
                regex.Match(String.Empty)
                |> ignore
                true
            with
            | :? ArgumentException ->
                false
    Expect.isTrue result "Is Regex well-formed?"

let expectSuccessfulMatch (str : string) (regex : string) =
    let result = Regex.IsMatch(str, regex)
    Expect.isTrue result "Did Regex match successfully?"

let testRuleset name (abnfString : string) testRule testData =
    testList name [
        let parser f () =
            let ruleMap =
                (abnfString.Trim())
                |> parseAllRules
                |> unwrap
                |> generate
            f ruleMap.[testRule]
        yield! testFixture parser [
            yield "Is Regex well-formed?", expectWellFormedRegex
            yield! testData |> List.map (fun datum -> sprintf "Does it match '%s'?" datum, expectSuccessfulMatch datum)
        ]
    ]