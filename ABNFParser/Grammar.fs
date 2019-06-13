[<AutoOpen>]
module ABNF.Grammar
open ABNF
open FParsec
open System

let isDebugMode = false

let (<!>) (p: Parser<_>) label : Parser<_> =
    if not isDebugMode then
        p
    else
        fun stream ->
            printfn "%A: Entering %s |%s|" stream.Position label (stream.PeekString(int stream.Column + int stream.IndexOfLastCharPlus1))
            let reply = p stream
            printfn "%A: Leaving %s (%A)|%s|" stream.Position label reply.Status (stream.PeekString(int stream.Column + int stream.IndexOfLastCharPlus1))
            reply

let pSpace = pchar ' '

type TerminalType =
    | Bit
    | Digit
    | HexDig

let pTerminalValueAs terminalType =
    match terminalType with
    | Bit    -> many1Chars (anyOf BIT)    |>> (fun bits   -> char <| Convert.ToInt32(bits,    2))
    | Digit  -> many1Chars (anyOf DIGIT)  |>> (fun digits -> char <| Convert.ToInt32(digits, 10))
    | HexDig -> many1Chars (anyOf HEXDIG) |>> (fun hex    -> char <| Convert.ToInt32(hex,    16))

let pTerminalType : Parser<_> =
    choice
        [
            (pchar 'b' >>. preturn Bit)
            (pchar 'd' >>. preturn Digit)
            (pchar 'x' >>. preturn HexDig)
        ]

let pTerminal : Parser<_> =
    let p =
        fun stream ->
            let reply = (pchar '%' >>. pTerminalType) stream
            if reply.Status = ReplyStatus.Ok then
                let reply' = pTerminalValueAs reply.Result stream
                if reply'.Status = ReplyStatus.Ok then
                    Reply((reply.Result, char reply'.Result))
                else
                    Reply(reply'.Status, reply'.Error)
            else
                Reply(reply.Status, reply.Error)
    p <!> "pTerminal"

let pTerminals : Parser<_> =
    let p =
        fun stream ->
            let reply = pTerminal stream
            if reply.Status = ReplyStatus.Ok then
                let (terminalType, result) = reply.Result
                let reply' = many ((pchar '.') >>. pTerminalValueAs terminalType) stream
                if reply'.Status = ReplyStatus.Ok then
                    Reply(Terminals (result :: reply'.Result))
                else
                    Reply(reply'.Status, reply'.Error)
            else
                Reply(reply.Status, reply.Error)
    p <!> "pTerminals"

let pTerminalRange : Parser<_> =
    let p =
        fun stream ->
            let reply = pTerminal stream
            if reply.Status = ReplyStatus.Ok then
                let (terminalType, result) = reply.Result
                let reply' = ((pchar '-') >>. pTerminalValueAs terminalType) stream
                if reply'.Status = ReplyStatus.Ok then
                    Reply(Terminals ([ for i in [result..reply'.Result] do yield char i ]))
                else
                    Reply(reply'.Status, reply'.Error)
            else
                Reply(reply.Status, reply.Error)
    p <!> "pTerminalRange"

let ruleChars = ALPHA @ DIGIT @ ['-'; '<'; '>']

let pWhitespace : Parser<_> =
    many (anyOf WSP)
    <!> "pWhitespace"

let pRuleName : Parser<_> =
    many1Chars (anyOf ruleChars)
    <!> "pRuleName"

let pRuleReference : Parser<_> =
    pRuleName
    |>> RuleReference
    <!> "pRuleReference"

let pComment : Parser<_> =
    pchar ';'
    >>. restOfLine false
    |>> Comment
    <!> "pComment"

let pString : Parser<_> =
    (pchar '"') >>. (many1Till anyChar (pchar '"'))
    |>> Terminals
    <!> "pString"

let pCoreRule : Parser<_> =
    let terminals charList =
        charList
        |> List.map (fun c -> Terminals [c])

    choice
        [
            stringReturn "ALPHA"  (Alternatives (terminals ALPHA))
            stringReturn "DIGIT"  (Alternatives (terminals DIGIT))
            stringReturn "HEXDIG" (Alternatives (terminals HEXDIG))
            stringReturn "DQUOTE" (Alternatives [Terminals [DQUOTE]])
            stringReturn "SP"     (Alternatives [Terminals [SP]])
            stringReturn "HTAB"   (Alternatives [Terminals [HTAB]])
            stringReturn "WSP"    (Alternatives (terminals WSP))
            //TODO: LWSP?
            stringReturn "VCHAR"  (Alternatives (terminals VCHAR))
            stringReturn "CHAR"   (Alternatives (terminals CHAR))
            stringReturn "OCTET"  (Alternatives (terminals OCTET))
            stringReturn "CTL"    (Alternatives (terminals CTL))
            stringReturn "CRLF"   (Terminals [CR; LF])
            stringReturn "CR"     (Alternatives [Terminals [CR]])
            stringReturn "LF"     (Alternatives [Terminals [LF]])
            stringReturn "BIT"    (Alternatives (terminals BIT))
        ]
    .>> followedBy (skipAnyOf ([ '['; ']'; '('; ')'; ' '; ';']) <|> eof)
    <!> "pCoreRule"

let (pRuleElement, pRuleElementRef) : (Parser<Element> * Parser<Element> ref) = createParserForwardedToRef()

let (pNotSequence, pNotSequenceRef) : (Parser<Element> * Parser<Element> ref) = createParserForwardedToRef()
let pSequence : Parser<_> =
    sepBy1 pNotSequence pSpace
    |>> Sequence
    <!> "pSequence"

let pOptionalGroup : Parser<_> =
    between (pchar '[') (pchar ']') pSequence
    |>> OptionalSequence
    <!> "pOptionalGroup"

let (pNotAlternates, pNotAlternatesRef) : (Parser<Element> * Parser<Element> ref) = createParserForwardedToRef()
let pAlternates : Parser<_> =
    let p =
        fun stream ->
            let reply = sepBy1 pNotAlternates (pWhitespace >>? (pchar '/') >>. pWhitespace) stream
            if reply.Status = ReplyStatus.Ok then
                let result =
                    match reply.Result with
                    | [ element ] -> element
                    | alternatives -> Alternatives alternatives
                Reply(result)
            else
                Reply(reply.Status, reply.Error)
    p <!> "pAlternates"

let pSequenceGroup : Parser<_> =
    between (pchar '(') (pchar ')') pSequence
    <!> "pSequenceGroup"

let pRepetition : Parser<_> =
    let p : Parser<_> =
        fun stream ->
            let reply =
                (tuple3
                    (opt puint8)
                    (opt (pchar '*'))
                    (opt puint8)) stream
            if reply.Status = ReplyStatus.Ok then
                match reply.Result with
                // 1
                | (Some startNum, None  , None) ->
                    Reply({ Start = Some startNum; IsRange = false; End = None })
                // 1*
                | (Some num,      Some _, None) ->
                    Reply({ Start = Some num;      IsRange = true;  End = None })
                // 1*2
                | (Some startNum, Some _, Some endNum) ->
                    Reply({ Start = Some startNum; IsRange = true;  End = Some endNum })
                //  *2
                | (None         , Some _, Some endNum) ->
                    Reply({ Start = None;          IsRange = true;  End = Some endNum })
                //  *
                | (None         , Some _, None) ->
                    Reply({ Start = None;          IsRange = true;  End = None })
                | _ ->
                    Reply(ReplyStatus.Error, ErrorMessageList(Expected("String in format of 2, 2*4, *4, *")))
            else
                Reply(reply.Status, reply.Error)
    p <!> "pRepetition"

let pNotAlternatesWithRepetition =
    pRepetition .>>. pNotAlternates
    |>> Repetition
    <!> "pNotAlternatesWithRepetition"

//Strings, names formation
//Comment
//Value range
//Repetition
//Grouping, optional
//Concatenation
//Alternative
do pRuleElementRef :=
    choice
        [
            (pSequenceGroup <|> pOptionalGroup)
            pAlternates
            (pTerminals <|> pTerminalRange <|> pCoreRule)
            pRuleReference
            pString
        ]
    <!> "pRuleElement"

do pNotAlternatesRef :=
    choice
        [
            pNotAlternatesWithRepetition
            (pSequenceGroup <|> pOptionalGroup)
            (pTerminals <|> pTerminalRange <|> pCoreRule)
            pRuleReference
            pString
        ]
    <!> "pNotAlternates"

do pNotSequenceRef :=
    choice
        [
            (pSequenceGroup <|> pOptionalGroup)
            pAlternates
            (pTerminals <|> pTerminalRange <|> pCoreRule)
            pString
        ]
    <!> "pRuleElement"

let pRule : Parser<_> =
    pRuleName
    .>>  pWhitespace
    .>>  pchar '='
    .>>  optional (pchar '/')
    .>>  pWhitespace
    .>>. sepBy1 pRuleElement (pchar ' ' .>>.? notFollowedBy (anyOf [ ' '; ';' ]))
    .>>  pWhitespace
    .>>  ((pComment |>> ignore) <|> (newline |>> ignore) <|> eof)
