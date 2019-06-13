#load @"references.fsx"
open System
open FParsec
open ABNF.Types
open ABNF.CoreRules
open ABNF.Grammar

run pRule """last-name        = *ALPHA ; this is a test comment"""

failwith "End"

let CompleteRuleParsingTest =
    [
        """postal-address   = name-part street zip-part"""
        """name-part        = *(personal-part SP) last-name [SP suffix] CRLF"""
        """name-part        =/ personal-part CRLF"""
        """personal-part    = first-name / (initial ".")"""
        """first-name       = *ALPHA"""
        """initial          = ALPHA"""
        """last-name        = *ALPHA ; this is a test comment"""
        """suffix           = ("Jr." / "Sr." / 1*("I" / "V" / "X"))"""
        """street           = [apt SP] house-num SP street-name CRLF"""
        """apt              = 1*4DIGIT"""
        """house-num        = 1*8(DIGIT / ALPHA)"""
        """street-name      = 1*VCHAR"""
        """zip-part         = town-name "," SP state 1*2SP zip-code CRLF"""
        """town-name        = 1*(ALPHA / SP)"""
        """state            = 2ALPHA"""
        """zip-code         = 5DIGIT ["-" 4DIGIT]"""
    ]
    |> List.map (fun str ->
        match run pRule str with
        | Success(result,_,_) ->
            let name,_ = result
            printfn "%s : %s" name str
        | _ ->
            printfn "ERR: %s" str)

let RulesWithRepetitionParsingTest =
    [
        "test-rule0 = 1*2\"2\" 2*3\"3\""
        "test-rule1 = 1*2\"2\" 2*3test-reference"
        "test-rule2 = 1*2(\"2\" / test-reference) 2*3[\"2\" / test-reference]"
        "test-rule3 = 1*2(\"2\" / \"3\") 2*3[\"2\" / \"3\"]"
        "test-rule4 = 1*2[\"2\" / \"3\"] 2*3(\"2\" / \"3\")"
    ]
    |> List.map (fun str ->
        run pRule str
        |> printfn "%A")
failwith "End"

let ElementsWithRepetitionParsingTest =
    [
        "1*2test-rule"
        "1*2\"2\""
        "1*2(\"2\" / \"3\")"
        "1*2[\"2\" / \"3\"]"
        "*2\"2\""
        "*2(\"2\" / \"3\")"
        "*2(\"2\" / \"3\")"
        "*2[\"2\" / \"3\"]"
        "2*\"2\""
        "2*(\"2\" / \"3\")"
        "2*[\"2\" / \"3\"]"
        "*\"2\""
        "*(\"2\" / \"3\")"
        "*[\"2\" / \"3\"]"
    ]
    |> List.map (fun str ->
        run pNotAlternatesWithRepetition str
        |> printfn "%A")
failwith "End"

let RuleParsingTest =
    [
        "TEST = \"0\" \"1\" \"2\" \"3\" \"4\" \"5\" \"6\" \"7\""
        "TEST = \"0\" / \"1\" / \"2\" / \"3\" / \"4\" / \"5\" / \"6\" / \"7\""
        "TEST = \"0\""
        "TEST = \"0\" / (\"1\" \"2\") / \"3\" / (\"4\" / \"5\") / \"6\" / \"7\""
        "TEST = \"0\" (\"1\" / \"2\") \"3\" (\"4\" / \"5\") \"6\" \"7\""
        "TEST = (\"1\" / \"2\") \"3\" (\"4\" / \"5\") \"6\" \"7\""
    ]
    |> List.map (fun str ->
        let res = run pRule str
        res
        |> printfn "%A")

let AlternatesParsingTest =
    run pAlternates "\"0\" / (\"1\" / \"2\") / \"3\""

let TerminalParsingTest =
    [
       "%b00010100‬"
       "%d20"
       "%x14"
    ]
    |> List.map (fun str ->
        run pTerminal str)

let TerminalsParsingTest =
    [
       "%b00010100.00011110.11111111"
       "%d20.30.255"
       "%x14.1E.FF"
    ]
    |> List.map (fun str ->
        run pTerminals str)

let TerminalRangeParsingTest =
    [
       "%b0000000-11111111"
       "%d0-255"
       "%x0-FF"
    ]
    |> List.map (fun str ->
        run pTerminalRange str)

let StringParsingTest =
    [
        @"""this is a string"""
        @"""this"""
        @"""thisa235hfnv"""
    ]
    |> List.map (fun str ->
        run pString str)

let RepetitionParsingTest =
    [
        @"2"
        @"*4"
        @"2*4"
        @"*"
        @"invalid"
    ]
    |> List.map (fun str ->
        run pRepetition str)

let CoreRuleParsingTest =
    [
        "ALPHA"
        "DIGIT"
        "HEXDIG"
        "DQUOTE"
        "SP"
        "HTAB"
        "WSP"
        //"LWSP"
        "VCHAR"
        "CHAR"
        "OCTET"
        "CTL"
        "CR"
        "LF"
        //"CRLF"
        "BIT"
    ]
    |> List.map (fun str ->
        run pCoreRule str)