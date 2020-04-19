module Generator

open FParsec
open System
open System.Text
open System.Text.RegularExpressions

let findRule (rules : Rule list) name =
    let definitions =
        rules
        |> List.filter (fun (rule : Rule) ->
            String.Equals(rule.RuleName, name, StringComparison.OrdinalIgnoreCase))

    let concatenatedRules =
        match definitions with
        | [ ] ->
            failwithf "%s was not defined prior to this point!" name
        | [ definition ] ->
            definition.Definition
        | _ ->
            definitions
            |> List.map (fun rule -> Sequence rule.Definition)
            |> Alternatives
            |> List.singleton

    { RuleName = name; Definition = concatenatedRules }

let validGroupNameCharacters = ['A'..'Z'] @ ['a'..'z'] @ [ '_' ]

let concatRules = String.concat ""
let concatAlternates = String.concat "|" >> sprintf "(?:%s)"
let realRegexEscape str =
    let escaped = Regex.Escape str
    escaped
        .Replace("]", "\]")
        .Replace("}", "\}")
        .Replace("/", "\/")
let rec generate (rules : Rule list) =
    let rec generateNext (element : RuleElement) : string =
        match element with
        | Terminals        terminals ->
            terminals
            |> List.map (string >> realRegexEscape)
            |> concatRules
        | Alternatives     elements ->
            if List.forall (fun e ->
                match e with
                | Terminals [ _ ] -> true
                | _ -> false) elements then
                elements
                |> List.map generateNext
                |> String.concat ""
                |> sprintf "[%s]"
            else
                elements
                |> List.map generateNext
                |> concatAlternates
        | OptionalSequence element ->
            element
            |> generateNext
            |> sprintf "(?:%s)?"
        | Sequence         elements ->
            elements
            |> List.map generateNext
            |> concatRules
            |> sprintf "(?:%s)"
        // I know, I know. Imperative BS. But, I'm lazy and it was easier this way ¯\_(ツ)_/¯
        | Repetition       (range, element) ->
            let makeRange str =
                match range with
                | Any                -> sprintf "%s*?" str
                | AtLeast 1uy        -> sprintf "%s+?" str
                | AtLeast atLeast    -> sprintf "%s{%d,}?" str atLeast
                | AtMost atMost      -> sprintf "%s{0,%d}?" str atMost
                | Exactly exactly    -> sprintf "%s{%d}" str exactly
                | Between (min, max) -> sprintf "%s{%d,%d}?" str min max

            element
            |> generateNext
            |> makeRange
        | RuleReference    name ->
            let groupName =
                name
                |> String.map (fun c ->
                    if validGroupNameCharacters |> List.contains c then c else '_')
            // would seriously benefit from memoization, but...
            (findRule rules name).Definition
            |> List.map generateNext
            |> concatRules
            |> sprintf "(?'%s'%s)" groupName
    rules
    |> List.rev // makes it easier to ignore rule definition order
    |> List.map (fun rule -> rule.RuleName, rule.Definition |> List.map generateNext |> concatRules)
    // handling duplicate rules / alternate rule cases
    |> List.groupBy fst
    |> List.map (fun (name, rules) ->
        (name, rules |> List.map snd |> concatAlternates |> sprintf "^%s$"))
    |> Map.ofList
