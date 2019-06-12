#r @"..\packages\FParsec\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec\lib\net40-client\FParsec.dll"
open FParsec

type Parser<'t> = Parser<'t, unit>

type Element =
    | Alternates of Element list
    | SequenceGroup of Element list
    | ParsedString of string

let (pRuleElement, pRuleElementRef) : (Parser<Element> * Parser<Element> ref) = createParserForwardedToRef()
let (pNotAlternatives, pNotAlternativesRef) : (Parser<Element> * Parser<Element> ref) = createParserForwardedToRef()

let pString =
    pchar '"' >>. manyCharsTill (noneOf ['"']) (pchar '"')
    |>> ParsedString

let pAlternates : Parser<_> =
    pipe2
        (pNotAlternatives .>>? (many (pchar ' ') >>? (pchar '/') >>. many (pchar ' ')))
        (sepBy1 pNotAlternatives (many (pchar ' ') >>? (pchar '/') >>. many (pchar ' ') ))
        (fun first rest -> first :: rest)
    |>> Alternates

let pSequenceGroup : Parser<_> =
    between (pchar '(') (pchar ')') (sepBy1 pRuleElement (pchar ' '))
    |>> SequenceGroup

do pRuleElementRef :=
    choice
        [
            pSequenceGroup
            pAlternates
            pString
        ]

do pNotAlternativesRef :=
    choice
        [
            pSequenceGroup
            pString
        ]

"\"0\" / (\"1\" \"2\") / \"3\" / (\"4\" / \"5\") / \"6\" / \"7\""
|> run (pRuleElement .>>? (skipNewline <|> eof))
