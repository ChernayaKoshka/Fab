module ABNF.Execution
open System
open FParsec
open System.Data

let parse (text : string) =
    run pDocument text

let findRule (rules : ABNFRule list) name =
    rules
    |> List.find (fun (rule : ABNFRule) ->
        rule.RuleName = name)

let rec matchElements (rules : ABNFRule list) (str : RuleStream) (elements : RuleElement list) =

    let rec matchElement (str : RuleStream) (element : RuleElement) =
        match element with
        | Terminals        terminals ->
            terminals
            |> List.fold (fun ((isMatch, rs) : bool * RuleStream) (terminal : Terminal) ->
                if isMatch && rs.Peek() = terminal then
                    (true, { rs with Pos = rs.Pos + 1 })
                else
                    (false, rs)) (true, str)
        | Alternatives     elements ->
            (true, str)
        | OptionalSequence element ->
            (true, str)
        | Group            elements ->
            (true, str)
        | Sequence         elements ->
            (true, str)
        | Repetition       (range, element) ->
            (true, str)
        | RuleReference    string ->
            (true, str)
    matchElement str elements.[0]

//let process (rules : ABNFRule list) (rulename : string) (text : string) =
//    let startingRule = findRule rules rulename
//    matchElement rules { Text = text; Pos = 0 } startingRule.Definition

