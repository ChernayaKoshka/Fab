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

let concatRules = String.concat ""

let rec generate (rules : Rule list) =
    let rec generateNext (element : RuleElement) : string =
        match element with
        | Terminals        terminals ->
            terminals
            |> List.map (string >> Regex.Escape)
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
                |> String.concat "|"
                |> sprintf "(?:%s)"
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
            let min, max =
                match range with
                | Any                -> (0uy    , Byte.MaxValue)
                | AtLeast (atLeast)  -> (atLeast, Byte.MaxValue)
                | AtMost (atMost)    -> (0uy    , atMost        )
                | Exactly (exactly)  -> (exactly, exactly       )
                | Between (min, max) -> (min    , max           )

            sprintf "%s{%d,%d}" (generateNext element) min max
        | RuleReference    name ->
            // would seriously benefit from memoization, but...
            (findRule rules name).Definition
            |> List.map generateNext
            |> concatRules
            |> sprintf "(?:(?#%s)%s)" name
    rules
    |> List.rev // makes it easier to ignore rule definition order
    |> List.map (fun rule -> rule.RuleName, rule.Definition |> List.map generateNext |> concatRules)
    |> Map.ofList
