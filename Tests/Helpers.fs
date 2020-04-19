[<RequireQualifiedAccess>]
module Helpers

open System
open System.Text.RegularExpressions
open FParsec
open Expecto
open Expecto.FParsec

let unwrap parseResult =
    match parseResult with
    | Success(res, _, _) ->
        res
    | Failure (err, _, _) ->
        failwithf "%A" err

let run parser =
    runParserOnString parser List.empty ""

let parseAndCompare parser data =
    fun () ->
        data
        |> List.iter (fun (input, expected) ->
            let res = run parser input
            Expect.isSuccess res "Did the parsing succeed?"
            Expect.equal expected (unwrap res) "Did the result match our expected output?") 

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

let expectSuccessfulMatch (regex : string) (str : string) =
    let result = Regex.IsMatch(str, regex)
    Expect.isTrue result "Did Regex match successfully?"

let testRuleset (abnfString : string) testRule testData =
    fun () ->
        let parser =
            let ruleMap =
                (abnfString.Trim())
                |> parseAllRules
                |> unwrap
                |> generate
            ruleMap.[testRule]
        expectWellFormedRegex parser
        testData
        |> List.iter (expectSuccessfulMatch parser)