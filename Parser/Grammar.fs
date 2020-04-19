[<AutoOpen>]
module Grammar
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
    | Bit    -> many1Chars (anyOf CoreRules.BIT)    |>> (fun bits   -> char <| Convert.ToInt32(bits,    2))
    | Digit  -> many1Chars (anyOf CoreRules.DIGIT)  |>> (fun digits -> char <| Convert.ToInt32(digits, 10))
    | HexDig -> many1Chars (anyOf CoreRules.HEXDIG) |>> (fun hex    -> char <| Convert.ToInt32(hex,    16))

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

let ruleChars = CoreRules.ALPHA @ CoreRules.DIGIT @ ['-'; '<'; '>']
let pRuleChar = anyOf ruleChars

let skipWhitespace : Parser<_> =
    skipMany (anyOf CoreRules.WSP)
    <!> "pWhitespace"

let pRuleName : Parser<_> =
    many1Chars pRuleChar
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
    between (pchar '"') (pchar '"') (many (noneOf [ '"' ]))
    |>> Terminals
    <!> "pString"

let pCoreRule : Parser<_> =
    choice
        [
            stringReturn "ALPHA" ALPHA   // (Alternatives (terminals ALPHA))
            stringReturn "DIGIT" DIGIT   // (Alternatives (terminals DIGIT))
            stringReturn "HEXDIG" HEXDIG // (Alternatives (terminals HEXDIG))
            stringReturn "DQUOTE" DQUOTE // (Terminals [DQUOTE])
            stringReturn "SP" SP         // (Terminals [SP])
            stringReturn "HTAB" HTAB     // (Terminals [HTAB])
            stringReturn "WSP" WSP       // (Alternatives (terminals WSP))
            stringReturn "VCHAR" VCHAR   // (Alternatives (terminals VCHAR))
            stringReturn "CHAR" CHAR     // (Alternatives (terminals CHAR))
            stringReturn "OCTET" OCTET   // (Alternatives (terminals OCTET))
            stringReturn "CTL" CTL       // (Alternatives (terminals CTL))
            stringReturn "CRLF" CRLF     // (Terminals [CR; LF])
            stringReturn "CR" CR         // (Terminals [CR])
            stringReturn "LF" LF         // (Terminals [LF])
            stringReturn "BIT" BIT       // (Alternatives (terminals BIT))
        ]
    |>> RuleElement.CoreRule
    // Prevent parsing parts of a rule reference by mistake (ex: SPOON, where SP would be parsed as a CoreRule by mistake)
    .>>? notFollowedBy pRuleChar
    <!> "pCoreRule"

let (pRuleElement, pRuleElementRef) : (Parser<RuleElement> * Parser<RuleElement> ref) = createParserForwardedToRef()

let (pNotSequence, pNotSequenceRef) : (Parser<RuleElement> * Parser<RuleElement> ref) = createParserForwardedToRef()
let pSequence : Parser<_> =
    sepEndBy1 pNotSequence pSpace
    |>> Sequence
    <!> "pSequence"

let pBetweenWhitespace openStr closeStr parser : Parser<_> =
    between (skipString openStr) (skipString closeStr) (skipWhitespace >>. parser .>> skipWhitespace)

let pOptionalGroup : Parser<_> =
    pBetweenWhitespace "[" "]" pSequence
    |>> OptionalSequence
    <!> "pOptionalGroup"

let pAlternateSeparator : Parser<_> =
    skipWhitespace >>? skipChar '/' .>> skipWhitespace

let (pNotAlternates, pNotAlternatesRef) : (Parser<RuleElement> * Parser<RuleElement> ref) = createParserForwardedToRef()
let pAlternates : Parser<_> =
    sepBy1 pNotAlternates pAlternateSeparator
    |>> (fun result ->
        match result with
        | [ element ] -> element
        | alternatives -> Alternatives alternatives)
    <!> "pAlternates"

let pSequenceGroup : Parser<_> =
    pBetweenWhitespace "(" ")" pSequence
    <!> "pSequenceGroup"

let pRepetition : Parser<_> =
    [
        // 1*2
        (puint8 .>> skipChar '*' .>>. puint8 ) |>> Between

        // *2
        (skipChar '*' >>. puint8) |>> AtMost

        // 1*
        (puint8 .>> skipChar '*') |>> AtLeast

        // *
        charReturn '*' Any

        // 1
        (puint8) |>> Exactly
    ]
    |> List.map attempt
    |> choice
    <!> "pRepetition"

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

let addRule (definition : Rule) : Parser<_> =
    updateUserState (fun us -> definition.RuleName :: us)
    >>% definition

let ruleMustBeDefined ruleName : Parser<_> =
    (userStateSatisfies (List.contains ruleName)
    >>% ruleName)
    <?> sprintf "'%s' was not defined prior to this point." ruleName


let pRuleDefinition : Parser<_> =
    sepBy1 pAlternates (pchar ' ' .>>.? notFollowedBy (anyOf [ ' '; ';' ]))
    .>>  skipWhitespace
    .>>  ((pComment |>> ignore) <|> (followedBy ((newline |>> ignore) <|> eof)))
    <!> "pRule"

let pRule : Parser<_> =
    pRuleName
    .>>  skipWhitespace
    .>>  pchar '='
    .>>?  skipWhitespace
    .>>.? pRuleDefinition
    <!> "pRule"

let pAlternateRule : Parser<_> =
    pRuleName
    >>= ruleMustBeDefined
    .>>  skipWhitespace
    .>>  pstring "=/"
    .>>  skipWhitespace
    .>>. pRuleDefinition
    <!> "pAlternateRule"

let pRuleRecord : Parser<_> =
    (pRule <|> pAlternateRule)
    |>> (fun (name, elements) ->
        { RuleName = name; Definition = elements })
    >>= addRule

let pDocument : Parser<_> =
    many1Till (pRuleRecord .>> (many (newline <|> pSpace))) eof
    <!> "pDocument"

let parseAllRules (text : string) =
    runParserOnString pDocument [ ] "" text