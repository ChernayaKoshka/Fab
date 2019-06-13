module Tests

open Expecto
open Expecto.FParsec
open ABNF
open FParsec
open Expecto.Flip

[<Tests>]
let simple =
    testList "simple parsing tests" [
        testList "terminal parsing test"
           ([
               "%b00010100‬"
               "%d20"
               "%x14"
            ]
            |> List.map (fun str ->
                testCase (sprintf "terminal parsing test: %s" str) <| fun _ ->
                    Expect.isSuccess (run pTerminal str) "What is this field for?"))

        testList "terminals parsing tests"
           ([
                "%b00010100.00011110.11111111"
                "%d20.30.255"
                "%x14.1E.FF"
            ]
            |> List.map (fun str ->
                testCase (sprintf "terminals parsing test: %s" str) <| fun _ ->
                    Expect.isSuccess (run pTerminals str) "What is this field for?"))

        testList "terminal range parsing tests"
           ([
                "%b0000000-11111111"
                "%d0-255"
                "%x0-FF"
            ]
            |> List.map (fun str ->
                testCase (sprintf "terminal range parsing test: %s" str) <| fun _ ->
                    Expect.isSuccess (run pTerminalRange str) "What is this field for?"))

        testList "string parsing tests"
           ([
                @"""this is a string"""
                @"""this"""
                @"""thisa235hfnv"""
            ]
            |> List.map (fun str ->
                testCase (sprintf "string parsing test: %s" str) <| fun _ ->
                    Expect.isSuccess (run pString str) "What is this field for?"))

        testList "repetition parsing tests"
           ([
                @"2"
                @"*4"
                @"2*4"
                @"*"
            ]
            |> List.map (fun str ->
                testCase (sprintf "repetition parsing test: %s" str) <| fun _ ->
                    Expect.isSuccess (run pRepetition str) "What is this field for?"))

        testList "core rule parsing tests"
           ([
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
                "CRLF"
                "BIT"
            ]
            |> List.map (fun str ->
                testCase (sprintf "core rule parsing test: %s" str) <| fun _ ->
                    Expect.isSuccess (run pCoreRule str) "What is this field for?"))
    ]

[<Tests>]
let groups =
    testList "group parsing tests" [
        testList "sequence parsing tests"
           ([
               @"%d1"
               @"%d1 %d2"
               @"%d1 %d2 / %d3"
               @"""1"" ""2"" ""3"""
            ]
            |> List.map (fun str ->
                testCase (sprintf "sequence parsing test: %s" str) <| fun _ ->
                    Expect.isSuccess (run pSequence str) "What is this field for?"))

        testList "sequence group parsing tests"
           ([
               @"(%d1)"
               @"(%d1 %d2)"
               @"(%d1 %d2 / %d3)"
               @"(""1"" ""2"" ""3"")"
            ]
            |> List.map (fun str ->
                testCase (sprintf "sequence group parsing test: %s" str) <| fun _ ->
                    Expect.isSuccess (run pSequenceGroup str) "What is this field for?"))

        testList "alternate parsing tests"
           ([
               @"%d1"
               @"%d1 / %d2"
               @"%d1 / %d2 / %d3"
               @"""1"" / ""2"" / ""3"""
            ]
            |> List.map (fun str ->
                testCase (sprintf "alternate parsing test: %s" str) <| fun _ ->
                    Expect.isSuccess (run pAlternates str) "What is this field for?"))

        testList "optional group parsing tests"
           ([
               @"[%d1]"
               @"[%d1 %d2]"
               @"[%d1 %d2 %d3]"
               @"[""1"" ""2"" ""3""]"
            ]
            |> List.map (fun str ->
                testCase (sprintf "optional group parsing test: %s" str) <| fun _ ->
                    Expect.isSuccess (run pOptionalGroup str) "What is this field for?"))
    ]

[<Tests>]
let combinations =
    testList "combination parsing tests" [
        testList "group combination parsing test"
           ([
               "\"0\" / (\"1\" / \"2\") / \"3\""
               "[\"0\" / (\"1\" / \"2\")] / \"3\" \"4\""
               "\"0\" / [\"1\" / \"2\"] / \"3\""
               "%x20 / (\"1\" / \"2\") / %x20"
               "[%d20 / (\"1\" / \"2\")] / %b0101"
               "%b0101 / [\"1\" / \"2\"] / %x20"
               "[%b0101 / [\"1\" / \"2\"] / %x20]"
               "[%b0101 / (%x0F) / %x20]"
            ]
            |> List.map (fun str ->
                testCase (sprintf "group parsing test: %s" str) <| fun _ ->
                    Expect.isSuccess (run pAlternates str) "What is this field for?"))

        testList "group repetition parsing test"
           ([
                "1*2test-rule"
                "1*2\"2\""
                "1*2(\"2\" / \"3\")"
                "1*2[\"2\" / \"3\"]"
                "*2\"2\""
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
                testCase (sprintf "group repetition parsing test: %s" str) <| fun _ ->
                    Expect.isSuccess (run pNotAlternatesWithRepetition str) "What is this field for?"))
    ]

[<Tests>]
let rules =
    testList "rule parsing tests" [

        testList "basic rule parsing test"
           ([
                "TEST = \"0\" \"1\" \"2\" \"3\" \"4\" \"5\" \"6\" \"7\""
                "TEST = \"0\" / \"1\" / \"2\" / \"3\" / \"4\" / \"5\" / \"6\" / \"7\""
                "TEST = \"0\""
                "TEST = \"0\" / (\"1\" \"2\") / \"3\" / (\"4\" / \"5\") / \"6\" / \"7\""
                "TEST = \"0\" (\"1\" / \"2\") \"3\" (\"4\" / \"5\") \"6\" \"7\""
                "TEST = (\"1\" / \"2\") \"3\" (\"4\" / \"5\") \"6\" \"7\""
            ]
            |> List.map (fun str ->
                testCase (sprintf "group repetition parsing test: %s" str) <| fun _ ->
                    Expect.isSuccess (run pRule str) "What is this field for?"))

        testList "basic rule with repetition parsing test"
           ([
                "test-rule0 = 1*2\"2\" 2*3\"3\""
                "test-rule1 = 1*2\"2\" 2*3test-reference"
                "test-rule2 = 1*2(\"2\" / test-reference) 2*3[\"2\" / test-reference]"
                "test-rule3 = 1*2(\"2\" / \"3\") 2*3[\"2\" / \"3\"]"
                "test-rule4 = 1*2[\"2\" / \"3\"] 2*3(\"2\" / \"3\")"
            ]
            |> List.map (fun str ->
                testCase (sprintf "basic rule with repetition parsing test: %s" str) <| fun _ ->
                    Expect.isSuccess (run pRule str) "What is this field for?"))

        testList "complex rule parsing test"
           ([
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
                testCase (sprintf "complex rule parsing test: %s" str) <| fun _ ->
                    Expect.isSuccess (run pRule str) "What is this field for?"))

    ]