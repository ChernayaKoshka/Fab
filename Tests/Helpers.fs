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
            sprintf "The parsing of '%s' did not succeed!" input
            |> Expect.isSuccess res 
            
            let unwrapped = unwrap res
            sprintf "The parsed result of '%s' was not expected!" input
            |> Expect.equal unwrapped expected) 
            
open Fab
open Fab.Generator

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
    sprintf "The regular expression '%s' was not well-formed!" str
    |> Expect.isTrue result

let expectSuccessfulMatch (regex : string) (str : string) =
    let result = Regex.IsMatch(str, regex)
    sprintf "'%s' did not match '%s'" regex str
    |> Expect.isTrue result

let testRuleset (abnfString : string) testRule testData =
    fun () ->
        let parser =
            let ruleMap =
                (abnfString.Trim())
                |> parseAllRules
                |> function Result.Ok res -> res | Result.Error err -> failwith err
                |> generate
            ruleMap.[testRule]
        expectWellFormedRegex parser
        testData
        |> List.iter (expectSuccessfulMatch parser)