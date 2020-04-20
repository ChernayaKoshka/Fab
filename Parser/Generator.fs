module Fab.Generator

open System
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
    let cachedDefinitions = new System.Collections.Generic.Dictionary<RuleName, string>()
    let rec generateNext (element : RuleElement) : string =
        match element with
        | REString str ->
            // strings are case-insensitive in ABNF. We can save a few characters in the resulting regular expression by checking if
            // anything contained within is a letter. If there isn't a single letter in the string, we just return the string.
            let escaped = realRegexEscape str
            if String.exists Char.IsLetter str then
                sprintf "(?i)%s(?-i)" escaped
            else
                escaped
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
        | CoreRule rule ->
            match rule with
            | ALPHA -> "[A-za-z]"
            | DIGIT -> "[0-9]"
            | HEXDIG -> "[0-9A-F]"
            | DQUOTE -> "\""
            | SP -> "\ "
            | HTAB -> "\\t"
            | WSP -> "[ \\t]"
            | CR -> "\\r"
            | LF -> "\\n"
            | CRLF -> "\\r\\n"
            | LWSP -> "(?:[ \\t]|\\r\\n[ \\t])*"
            | VCHAR -> "[!-~]"
            | CHAR -> "[\\x01-\\x7E]"
            | OCTET -> "[\\x00-\\x7F]"
            | CTL -> "[\\x00-\\x1F\\x7F]"
            | BIT -> "[01]"
        | OptionalSequence element ->
            element
            |> generateNext
            |> sprintf "(?:%s)?"
        | Sequence         elements ->
            elements
            |> List.map generateNext
            |> concatRules
            |> sprintf "(?:%s)"
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
            match cachedDefinitions.TryGetValue(name.ToUpper()) with
            | (true, cachedExpression) -> 
                cachedExpression
            | (false, _) ->
                let groupName =
                    name
                    |> String.map (fun c ->
                        if validGroupNameCharacters |> List.contains c then c else '_')
                // would seriously benefit from memoization, but...
                let generated =
                    (findRule rules name).Definition
                    |> List.map generateNext
                    |> concatRules
                    |> sprintf "(?'%s'%s)" groupName
                cachedDefinitions.Add(name.ToUpper(), generated)
                generated

    rules
    |> List.rev // makes it easier to ignore rule definition order
    |> List.map (fun rule -> 
        match cachedDefinitions.TryGetValue(rule.RuleName.ToUpper()) with
        | (true, cached) -> rule.RuleName, cached
        | (false, _) -> rule.RuleName, rule.Definition |> List.map generateNext |> concatRules)
    // handling duplicate rules / alternate rule cases
    |> List.groupBy fst
    |> List.map (fun (name, rules) ->
        (name, rules |> List.map snd |> concatAlternates |> sprintf "^%s$"))
    |> Map.ofList
