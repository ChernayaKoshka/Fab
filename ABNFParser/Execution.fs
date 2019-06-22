module ABNF.Execution
open System
open FParsec
open System.Data
open System

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
                if isMatch && rs.Peek() = Next terminal then
                    (true, { rs with Pos = rs.Pos + 1 })
                else
                    (false, rs)) (true, str)
        | Alternatives     elements ->
            (true, str)
        | OptionalSequence element ->
            let result = matchElement str element
            if fst result then
                result
            else
                (true, str)
        | Group            elements ->
            (true, str)
        | Sequence         elements ->
            elements
            |> List.fold (fun (isMatch, stream) element ->
                if isMatch then
                    matchElement stream element
                else
                    (false, str)) (true, str)
        // I know, I know. Imperative BS. But, I'm lazy and it was easier this way ¯\_(ツ)_/¯
        | Repetition       (range, element) ->
            let mutable count = 0uy
            let mutable temp = (true, str)
            match range with
            | Any ->
                while fst temp do
                    temp <- matchElements rules (snd temp) [element]
                (true, snd temp)
            | AtLeast (atLeast) ->
                while fst temp do
                    temp <- matchElements rules (snd temp) [element]
                    if fst temp then
                        count <- count + 1uy
                if count >= atLeast then
                    (true, snd temp)
                else
                    (false, str)
            | AtMost (atMost) ->
                while fst temp && count + 1uy <= atMost do
                    temp <- matchElements rules (snd temp) [element]
                    if fst temp then
                        count <- count + 1uy
                if count <= atMost then
                    (true, snd temp)
                else
                    (false, str)
            | Exactly (exactly) ->
                let mutable shortCircuit = false
                while fst temp && not shortCircuit do
                    temp <- matchElements rules (snd temp) [element]
                    if fst temp then
                        count <- count + 1uy
                        if count = exactly then
                            shortCircuit <- true
                if count = exactly then
                    (true, snd temp)
                else
                    (false, str)
            | Between (min, max) ->
                let mutable shortCircuit = false
                while fst temp && not shortCircuit do
                    temp <- matchElements rules (snd temp) [element]
                    if fst temp then
                        count <- count + 1uy
                        if count = max then
                            shortCircuit <- true
                if count >= min && count <= max then
                    (true, snd temp)
                else
                    (false, str)
        | RuleReference    string ->
            (true, str)
    matchElement str elements.[0]

//let process (rules : ABNFRule list) (rulename : string) (text : string) =
//    let startingRule = findRule rules rulename
//    matchElement rules { Text = text; Pos = 0 } startingRule.Definition

