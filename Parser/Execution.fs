module Execution
open FParsec
let findRule (rules : Rule list) name =
    let definitions =
        rules
        |> List.filter (fun (rule : Rule) ->
            rule.RuleName = name)
    let concatenatedRules =
        match definitions with
        | [ definition ] ->
            definition.Definition
        | _ ->
            definitions
            |> List.map (fun rule -> Sequence rule.Definition)
            |> Alternatives
            |> List.singleton

    { RuleName = name; Definition = concatenatedRules }

let rec matchElements (rules : Rule list) (str : RuleStream) (elements : RuleElement list) =
    let rec matchElement (str : RuleStream) (element : RuleElement) =
        let isEndOfStream = str.Pos = str.Text.Length
        printfn "%s" (str.Text.Replace("\r", "\\r").Replace("\n", "\\n"))
        for z = 0 to str.Pos - 1 do
            printf "-"
        printfn "^"
        printfn "%s\r\n" (element.ToString())
        match element with
        | Terminals        terminals ->
            if isEndOfStream then
                (false, str)
            else
                terminals
                |> List.fold (fun ((isMatch, rs) : bool * RuleStream) (terminal : Terminal) ->
                    if isMatch && rs.Peek() = Next terminal then
                        (true, { rs with Pos = rs.Pos + 1 })
                    else
                        (false, rs)) (true, str)
        | Alternatives     elements ->
            let result =
                elements
                |> List.tryPick (fun e ->
                    let result = matchElement str e
                    if fst result then
                        Some result
                    else
                        None)
            match result with
            | Some res -> res
            | None -> (false, str)
        | OptionalSequence element ->
            let result = matchElement str element
            if fst result then
                result
            else
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
                    temp <- matchElement (snd temp) element
                (true, snd temp)
            | AtLeast (atLeast) ->
                while fst temp do
                    temp <- matchElement (snd temp) element
                    if fst temp then
                        count <- count + 1uy
                if count >= atLeast then
                    (true, snd temp)
                else
                    (false, str)
            | AtMost (atMost) ->
                while fst temp && count + 1uy <= atMost do
                    temp <- matchElement (snd temp) element
                    if fst temp then
                        count <- count + 1uy
                if count <= atMost then
                    (true, snd temp)
                else
                    (false, str)
            | Exactly (exactly) ->
                let mutable shortCircuit = false
                while fst temp && not shortCircuit do
                    temp <- matchElement (snd temp) element
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
                    temp <- matchElement (snd temp) element
                    if fst temp then
                        count <- count + 1uy
                        if count = max then
                            shortCircuit <- true
                if count >= min && count <= max then
                    (true, snd temp)
                else
                    (false, str)
        | RuleReference    string ->
            let rule =
                rules
                |> List.find (fun rule ->
                    rule.RuleName.ToUpper() = string.ToUpper())
            matchElements rules str rule.Definition

    let success, _, _, rs =
        elements
        |> List.fold (fun (success, index, doneParsing, rs) element ->
            if success && not doneParsing then
                let (result, doneParsing, rs') =
                    match element with
                    | OptionalSequence _ ->
                        //skip optional element and see how that pans out for us
                        let success',  rs' =
                            if index + 1 < elements.Length then
                                matchElements rules rs elements.[index + 1..]
                            else
                                (false, rs)
                        let success'', rs'' = matchElement rs element
                        match success', success'' with
                        | true,  true when rs'.Pos >= rs''.Pos -> (success'  , true,  rs'  )
                        | true,  true when rs''.Pos >= rs'.Pos -> (success'' , false, rs'' )
                        | true,  true  -> failwith "Impossible, just putting here to keep the compiler from bitching"
                        | false, true  -> (success'', false, rs'')
                        | true , false -> (success' , true,  rs' )
                        | false, false -> (success'', false, rs'')
                    | _ ->
                        let result, rs' = matchElement rs element
                        (result, false, rs')
                (result, index + 1, doneParsing, rs')
            else
                (success, index, doneParsing, rs)) (true, 0, false, str)
    (success, rs)

//let process (rules : Rule list) (rulename : string) (text : string) =
//    let startingRule = findRule rules rulename
//    matchElement rules { Text = text; Pos = 0 } startingRule.Definition

