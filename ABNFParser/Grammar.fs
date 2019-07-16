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
            printfn "%A: Entering %s" stream.Position label
            printfn "|%s|" (stream.PeekString(int stream.Column + int stream.IndexOfLastCharPlus1))
            let reply = p stream
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            printfn "|%s|" (stream.PeekString(int stream.Column + int stream.IndexOfLastCharPlus1))
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

                let reply' =
                    if stream.Peek() = '.' then
                        (many ((pchar '.') >>. pTerminalValueAs terminalType)
                        |>> (fun chars -> result :: chars)) stream
                    else if (stream.Peek() = '-') then
                        ((pchar '-') >>. pTerminalValueAs terminalType
                        |>> (fun range -> [ for i in [result..range] do yield char i ])) stream
                    else
                        Reply([result])

                if reply'.Status = ReplyStatus.Ok then
                    Reply(Terminals (reply'.Result))
                else
                    Reply(reply'.Status, reply'.Error)
            else
                Reply(reply.Status, reply.Error)
    p <!> "pTerminals"

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
            stringReturn "DQUOTE" (Terminals [DQUOTE])
            stringReturn "SP"     (Terminals [SP])
            stringReturn "HTAB"   (Terminals [HTAB])
            stringReturn "WSP"    (Alternatives (terminals WSP))
            //TODO: LWSP?
            stringReturn "VCHAR"  (Alternatives (terminals VCHAR))
            stringReturn "CHAR"   (Alternatives (terminals CHAR))
            stringReturn "OCTET"  (Alternatives (terminals OCTET))
            stringReturn "CTL"    (Alternatives (terminals CTL))
            stringReturn "CRLF"   (Terminals [CR; LF])
            stringReturn "CR"     (Terminals [CR])
            stringReturn "LF"     (Terminals [LF])
            stringReturn "BIT"    (Alternatives (terminals BIT))
        ]
    .>> followedBy (skipAnyOf ([ '['; ']'; '('; ')'; ' '; ';']) <|> eof <|> skipNewline)
    <!> "pCoreRule"

let (pRuleElement, pRuleElementRef) : (Parser<RuleElement> * Parser<RuleElement> ref) = createParserForwardedToRef()

let (pNotSequence, pNotSequenceRef) : (Parser<RuleElement> * Parser<RuleElement> ref) = createParserForwardedToRef()
let pSequence : Parser<_> =
    sepEndBy1 pNotSequence pSpace
    |>> Sequence
    <!> "pSequence"

let pOptionalGroup : Parser<_> =
    between (pchar '[' .>> pWhitespace) (pWhitespace >>. pchar ']') pSequence
    |>> OptionalSequence
    <!> "pOptionalGroup"

let (pNotAlternates, pNotAlternatesRef) : (Parser<RuleElement> * Parser<RuleElement> ref) = createParserForwardedToRef()
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
    between (pchar '(' .>> pWhitespace) (pWhitespace >>. pchar ')') pSequence
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
                    Reply(Exactly startNum)
                // 1*
                | (Some num,      Some _, None) ->
                    Reply(AtLeast num)
                // 1*2
                | (Some startNum, Some _, Some endNum) ->
                    Reply(Between(startNum, endNum))
                //  *2
                | (None         , Some _, Some endNum) ->
                    Reply(AtMost endNum)
                //  *
                | (None         , Some _, None) ->
                    Reply(Any)
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
            (pTerminals <|> (attempt pCoreRule))
            pRuleReference
            pString
        ]
    <!> "pRuleElement"

do pNotAlternatesRef :=
    choice
        [
            pNotAlternatesWithRepetition
            (pSequenceGroup <|> pOptionalGroup)
            (pTerminals <|> (attempt pCoreRule))
            pRuleReference
            pString
        ]
    <!> "pNotAlternates"

do pNotSequenceRef :=
    choice
        [
            (pSequenceGroup <|> pOptionalGroup)
            pAlternates
            (pTerminals <|> (attempt pCoreRule))
            pString
        ]
    <!> "pRuleElement"

let pRule : Parser<_> =
    pRuleName
    .>>  pWhitespace
    .>>  pchar '='
    .>>  optional (pchar '/')
    .>>  pWhitespace
    .>>. sepBy1 pAlternates (pchar ' ' .>>.? notFollowedBy (anyOf [ ' '; ';' ]))
    .>>  pWhitespace
    .>>  ((pComment |>> ignore) <|> (followedBy ((newline |>> ignore) <|> eof)))
    <!> "pRule"

let pRuleRecord : Parser<_> =
    pRule
    |>> (fun (name, elements) -> { RuleName = name; Definition = elements })

let pDocument : Parser<_> =
    many1Till (pRuleRecord .>> (many (newline <|> pSpace))) eof
    <!> "pDocument"