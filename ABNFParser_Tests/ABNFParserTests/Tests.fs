module Tests

open Expecto
open Expecto.FParsec
open ABNF
open FParsec
open Expecto.Flip

let unwrap parseResult =
    match parseResult with
    | Success(res, _, _) -> res

[<Tests>]
let simple =
    testList "simple parsing tests" [
        testList "terminal parsing test"
           ([
                ("%b00010100‬", (Bit, '\020'))
                ("%d20", (Digit, '\020'))
                ("%x14", (HexDig, '\020'))
            ]
            |> List.map (fun (str, expected) ->
                testCase (sprintf "terminal parsing test: %s" str) <| fun _ ->
                    let res = (run pTerminal str)
                    Expect.isSuccess res "What is this field for?"
                    Expect.equal "What is this field for?" expected (unwrap res)))

        testList "terminals parsing tests"
           ([
                ("%b00010100.00011110.11111111", Terminals ['\020'; '\030'; 'ÿ'])
                ("%d20.30.255", Terminals ['\020'; '\030'; 'ÿ'])
                ("%x14.1E.FF", Terminals ['\020'; '\030'; 'ÿ'])
            ]
            |> List.map (fun (str, expected) ->
                testCase (sprintf "terminals parsing test: %s" str) <| fun _ ->
                    let res = (run pTerminals str)
                    Expect.isSuccess res "What is this field for?"
                    Expect.equal "What is this field for?" expected (unwrap res)))

        testList "terminal range parsing tests"
           ([
                ("%b0000000-11111111", Terminals ['\000'..'\255'])
                ("%d0-255", Terminals ['\000'..'\255'])
                ("%x0-FF", Terminals ['\000'..'\255'])
            ]
            |> List.map (fun (str, expected) ->
                testCase (sprintf "terminal range parsing test: %s" str) <| fun _ ->
                    let res = (run pTerminalRange str)
                    Expect.isSuccess res "What is this field for?"
                    Expect.equal "What is this field for?" expected (unwrap res)))

        testList "string parsing tests"
           ([
                ("\"this is a string\"", Terminals ['t'; 'h'; 'i'; 's'; ' '; 'i'; 's'; ' '; 'a'; ' '; 's'; 't'; 'r'; 'i'; 'n'; 'g'])
                ("\"this\"", Terminals ['t'; 'h'; 'i'; 's'])
                ("\"thisa235hfnv\"", Terminals ['t'; 'h'; 'i'; 's'; 'a'; '2'; '3'; '5'; 'h'; 'f'; 'n'; 'v'])
            ]
            |> List.map (fun (str, expected) ->
                testCase (sprintf "string parsing test: %s" str) <| fun _ ->
                    let res = (run pString str)
                    Expect.isSuccess res "What is this field for?"
                    Expect.equal "What is this field for?" expected (unwrap res)))

        testList "repetition parsing tests"
           ([
                ("2", {Start = Some 2uy; IsRange = false; End = None;})
                ("*4", {Start = None; IsRange = true; End = Some 4uy;})
                ("2*4", {Start = Some 2uy; IsRange = true; End = Some 4uy;})
                ("*", {Start = None; IsRange = true; End = None;})
            ]
            |> List.map (fun (str, expected) ->
                testCase (sprintf "repetition parsing test: %s" str) <| fun _ ->
                    let res = (run pRepetition str)
                    Expect.isSuccess res "What is this field for?"
                    Expect.equal "What is this field for?" expected (unwrap res)))

        testList "core rule parsing tests"
           ([
                ("ALPHA", Alternatives (ALPHA |> List.map (fun c -> Terminals [c])))
                ("DIGIT", Alternatives (DIGIT |> List.map (fun c -> Terminals [c])))
                ("HEXDIG", Alternatives (HEXDIG |> List.map (fun c -> Terminals [c])))
                ("DQUOTE", Alternatives ([Terminals [ DQUOTE ]]))
                ("SP", Alternatives ([Terminals [ SP ]]))
                ("HTAB", Alternatives ([Terminals [ HTAB ]]))
                ("WSP", Alternatives (WSP |> List.map (fun c -> Terminals [c])))
                ("VCHAR", Alternatives (VCHAR |> List.map (fun c -> Terminals [c])))
                ("CHAR", Alternatives (CHAR |> List.map (fun c -> Terminals [c])))
                ("OCTET", Alternatives (OCTET |> List.map (fun c -> Terminals [c])))
                ("CTL", Alternatives (CTL |> List.map (fun c -> Terminals [c])))
                ("CR", Alternatives ([Terminals [ CR ]]))
                ("LF", Alternatives ([Terminals [ LF ]]))
                ("CRLF", Terminals (List.ofArray <| CRLF.ToCharArray()))
                ("BIT", Alternatives (BIT |> List.map (fun c -> Terminals [c])))
            ]
            |> List.map (fun (str, expected) ->
                testCase (sprintf "core rule parsing test: %s" str) <| fun _ ->
                    let res = (run pCoreRule str)
                    Expect.isSuccess res "What is this field for?"
                    Expect.equal "What is this field for?" expected (unwrap res)))
    ]

[<Tests>]
let groups =
    testList "group parsing tests" [
        testList "sequence parsing tests"
           ([
                ("%d1", Sequence [Terminals ['\001']])
                ("%d1 %d2", Sequence [Terminals ['\001']; Terminals ['\002']])
                ("%d1 %d2 / %d3", Sequence [Terminals ['\001']; Alternatives [Terminals ['\002']; Terminals ['\003']]])
                ("\"1\" \"2\" \"3\"", Sequence [Terminals ['1']; Terminals ['2']; Terminals ['3']])
            ]
            |> List.map (fun (str, expected) ->
                testCase (sprintf "sequence parsing test: %s" str) <| fun _ ->
                    let res = (run pSequence str)
                    Expect.isSuccess res "What is this field for?"
                    Expect.equal "What is this field for?" expected (unwrap res)))

        testList "sequence group parsing tests"
           ([
                ("(%d1)", Sequence [Terminals ['\001']])
                ("(%d1 %d2)", Sequence [Terminals ['\001']; Terminals ['\002']])
                ("(%d1 %d2 / %d3)", Sequence [Terminals ['\001']; Alternatives [Terminals ['\002']; Terminals ['\003']]])
                ("(\"1\" \"2\" \"3\")", Sequence [Terminals ['1']; Terminals ['2']; Terminals ['3']])
            ]
            |> List.map (fun (str, expected) ->
                testCase (sprintf "sequence group parsing test: %s" str) <| fun _ ->
                    let res = (run pSequenceGroup str)
                    Expect.isSuccess res "What is this field for?"
                    Expect.equal "What is this field for?" expected (unwrap res)))

        testList "alternate parsing tests"
           ([
                ("%d1", Terminals ['\001'])
                ("%d1 / %d2", Alternatives [Terminals ['\001']; Terminals ['\002']])
                ("%d1 / %d2 / %d3", Alternatives [Terminals ['\001']; Terminals ['\002']; Terminals ['\003']])
                ("\"1\" / \"2\" / \"3\"", Alternatives [Terminals ['1']; Terminals ['2']; Terminals ['3']])
            ]
            |> List.map (fun (str, expected) ->
                testCase (sprintf "alternate parsing test: %s" str) <| fun _ ->
                    let res = (run pAlternates str)
                    Expect.isSuccess res "What is this field for?"
                    Expect.equal "What is this field for?" expected (unwrap res)))

        testList "optional group parsing tests"
           ([
                ("[%d1]", OptionalSequence (Sequence [Terminals ['\001']]))
                ("[%d1 %d2]", OptionalSequence (Sequence [Terminals ['\001']; Terminals ['\002']]))
                ("[%d1 %d2 %d3]", OptionalSequence (Sequence [Terminals ['\001']; Terminals ['\002']; Terminals ['\003']]))
                ("[\"1\" \"2\" \"3\"]", OptionalSequence (Sequence [Terminals ['1']; Terminals ['2']; Terminals ['3']]))
            ]
            |> List.map (fun (str, expected) ->
                testCase (sprintf "optional group parsing test: %s" str) <| fun _ ->
                    let res = (run pOptionalGroup str)
                    Expect.isSuccess res "What is this field for?"
                    Expect.equal "What is this field for?" expected (unwrap res)))
    ]

[<Tests>]
let combinations =
    testList "combination parsing tests" [
        testList "group combination parsing test"
           ([
                ("\"0\" / (\"1\" / \"2\") / \"3\"", Alternatives [Terminals ['0']; Sequence [Alternatives [Terminals ['1']; Terminals ['2']]]; Terminals ['3']])
                ("[\"0\" / (\"1\" / \"2\")] / \"3\" \"4\"", Alternatives [OptionalSequence (Sequence [Alternatives [Terminals ['0']; Sequence [Alternatives [Terminals ['1']; Terminals ['2']]]]]); Terminals ['3']])
                ("\"0\" / [\"1\" / \"2\"] / \"3\"", Alternatives [Terminals ['0']; OptionalSequence (Sequence [Alternatives [Terminals ['1']; Terminals ['2']]]); Terminals ['3']])
                ("%x20 / (\"1\" / \"2\") / %x20", Alternatives [Terminals [' ']; Sequence [Alternatives [Terminals ['1']; Terminals ['2']]]; Terminals [' ']])
                ("[%d20 / (\"1\" / \"2\")] / %b0101", Alternatives [OptionalSequence (Sequence [Alternatives [Terminals ['\020']; Sequence [Alternatives [Terminals ['1']; Terminals ['2']]]]]); Terminals ['\005']])
                ("%b0101 / [\"1\" / \"2\"] / %x20", Alternatives [Terminals ['\005']; OptionalSequence (Sequence [Alternatives [Terminals ['1']; Terminals ['2']]]); Terminals [' ']])
                ("[%b0101 / [\"1\" / \"2\"] / %x20]", OptionalSequence (Sequence [Alternatives [Terminals ['\005']; OptionalSequence (Sequence [Alternatives [Terminals ['1']; Terminals ['2']]]); Terminals [' ']]]))
                ("[%b0101 / (%x0F) / %x20]", OptionalSequence (Sequence [Alternatives [Terminals ['\005']; Sequence [Terminals ['\015']]; Terminals [' ']]]))
            ]
            |> List.map (fun (str, expected) ->
                testCase (sprintf "group parsing test: %s" str) <| fun _ ->
                    let res = (run pAlternates str)
                    Expect.isSuccess res "What is this field for?"
                    Expect.equal "What is this field for?" expected (unwrap res)))

        testList "group repetition parsing test"
           ([
                ("1*2test-rule", Repetition ({Start = Some 1uy; IsRange = true; End = Some 2uy;}, RuleReference "test-rule"))
                ("1*2\"2\"", Repetition ({Start = Some 1uy; IsRange = true; End = Some 2uy;}, Terminals ['2']))
                ("1*2(\"2\" / \"3\")", Repetition ({Start = Some 1uy; IsRange = true; End = Some 2uy;}, Sequence [Alternatives [Terminals ['2']; Terminals ['3']]]))
                ("1*2[\"2\" / \"3\"]", Repetition ({Start = Some 1uy; IsRange = true; End = Some 2uy;}, OptionalSequence (Sequence [Alternatives [Terminals ['2']; Terminals ['3']]])))
                ("*2\"2\"", Repetition ({Start = None; IsRange = true; End = Some 2uy;}, Terminals ['2']))
                ("*2(\"2\" / \"3\")", Repetition ({Start = None; IsRange = true; End = Some 2uy;}, Sequence [Alternatives [Terminals ['2']; Terminals ['3']]]))
                ("*2[\"2\" / \"3\"]", Repetition ({Start = None; IsRange = true; End = Some 2uy;}, OptionalSequence (Sequence [Alternatives [Terminals ['2']; Terminals ['3']]])))
                ("2*\"2\"", Repetition ({Start = Some 2uy; IsRange = true; End = None;}, Terminals ['2']))
                ("2*(\"2\" / \"3\")", Repetition ({Start = Some 2uy; IsRange = true; End = None;}, Sequence [Alternatives [Terminals ['2']; Terminals ['3']]]))
                ("2*[\"2\" / \"3\"]", Repetition ({Start = Some 2uy; IsRange = true; End = None;}, OptionalSequence (Sequence [Alternatives [Terminals ['2']; Terminals ['3']]])))
                ("*\"2\"", Repetition ({Start = None; IsRange = true; End = None;}, Terminals ['2']))
                ("*(\"2\" / \"3\")", Repetition ({Start = None; IsRange = true; End = None;}, Sequence [Alternatives [Terminals ['2']; Terminals ['3']]]))
                ("*[\"2\" / \"3\"]", Repetition ({Start = None; IsRange = true; End = None;}, OptionalSequence (Sequence [Alternatives [Terminals ['2']; Terminals ['3']]])))
            ]
            |> List.map (fun (str, expected) ->
                testCase (sprintf "group repetition parsing test: %s" str) <| fun _ ->
                    let res = (run pNotAlternatesWithRepetition str)
                    Expect.isSuccess res "What is this field for?"
                    Expect.equal "What is this field for?" expected (unwrap res)))
    ]

[<Tests>]
let rules =
    testList "rule parsing tests" [

        testList "basic rule parsing test"
           ([
                ("TEST = \"0\" \"1\" \"2\" \"3\" \"4\" \"5\" \"6\" \"7\"", ("TEST",
                     [Terminals ['0']; Terminals ['1']; Terminals ['2']; Terminals ['3'];
                      Terminals ['4']; Terminals ['5']; Terminals ['6']; Terminals ['7']]))
                ("TEST = \"0\" / \"1\" / \"2\" / \"3\" / \"4\" / \"5\" / \"6\" / \"7\"", ("TEST",
                     [Alternatives
                        [Terminals ['0']; Terminals ['1']; Terminals ['2']; Terminals ['3'];
                         Terminals ['4']; Terminals ['5']; Terminals ['6']; Terminals ['7']]]))
                ("TEST = \"0\"", ("TEST", [Terminals ['0']]))
                ("TEST = \"0\" / (\"1\" \"2\") / \"3\" / (\"4\" / \"5\") / \"6\" / \"7\"", ("TEST",
                     [Alternatives
                        [Terminals ['0']; Sequence [Terminals ['1']; Terminals ['2']];
                         Terminals ['3']; Sequence [Alternatives [Terminals ['4']; Terminals ['5']]];
                         Terminals ['6']; Terminals ['7']]]))
                ("TEST = \"0\" (\"1\" / \"2\") \"3\" (\"4\" / \"5\") \"6\" \"7\"", ("TEST",
                     [Terminals ['0']; Sequence [Alternatives [Terminals ['1']; Terminals ['2']]];
                      Terminals ['3']; Sequence [Alternatives [Terminals ['4']; Terminals ['5']]];
                      Terminals ['6']; Terminals ['7']]))
                ("TEST = (\"1\" / \"2\") \"3\" (\"4\" / \"5\") \"6\" \"7\"", ("TEST",
                     [Sequence [Alternatives [Terminals ['1']; Terminals ['2']]]; Terminals ['3'];
                      Sequence [Alternatives [Terminals ['4']; Terminals ['5']]]; Terminals ['6'];
                      Terminals ['7']]))
            ]
            |> List.map (fun (str, expected) ->
                testCase (sprintf "group repetition parsing test: %s" str) <| fun _ ->
                    let res = (run pRule str)
                    Expect.isSuccess res "What is this field for?"
                    Expect.equal "What is this field for?" expected (unwrap res)))

        testList "basic rule with repetition parsing test"
           ([
            ("test-rule0 = 1*2\"2\" 2*3\"3\"", ("test-rule0",
                 [Repetition ({Start = Some 1uy;
                               IsRange = true;
                               End = Some 2uy;},Terminals ['2']);
                  Repetition ({Start = Some 2uy;
                               IsRange = true;
                               End = Some 3uy;},Terminals ['3'])]))
            ("test-rule1 = 1*2\"2\" 2*3test-reference", ("test-rule1",
                 [Repetition ({Start = Some 1uy;
                               IsRange = true;
                               End = Some 2uy;},Terminals ['2']);
                  Repetition ({Start = Some 2uy;
                               IsRange = true;
                               End = Some 3uy;},RuleReference "test-reference")]))
            ("test-rule2 = 1*2(\"2\" / test-reference) 2*3[\"2\" / test-reference]", ("test-rule2",
                 [Repetition
                    ({Start = Some 1uy;
                      IsRange = true;
                      End = Some 2uy;},
                     Sequence [Alternatives [Terminals ['2']; RuleReference "test-reference"]]);
                  Repetition
                    ({Start = Some 2uy;
                      IsRange = true;
                      End = Some 3uy;},
                     OptionalSequence
                       (Sequence
                          [Alternatives [Terminals ['2']; RuleReference "test-reference"]]))]))
            ("test-rule3 = 1*2(\"2\" / \"3\") 2*3[\"2\" / \"3\"]", ("test-rule3",
                 [Repetition
                    ({Start = Some 1uy;
                      IsRange = true;
                      End = Some 2uy;},
                     Sequence [Alternatives [Terminals ['2']; Terminals ['3']]]);
                  Repetition
                    ({Start = Some 2uy;
                      IsRange = true;
                      End = Some 3uy;},
                     OptionalSequence
                       (Sequence [Alternatives [Terminals ['2']; Terminals ['3']]]))]))
            ("test-rule4 = 1*2[\"2\" / \"3\"] 2*3(\"2\" / \"3\")", ("test-rule4",
                 [Repetition
                    ({Start = Some 1uy;
                      IsRange = true;
                      End = Some 2uy;},
                     OptionalSequence
                       (Sequence [Alternatives [Terminals ['2']; Terminals ['3']]]));
                  Repetition
                    ({Start = Some 2uy;
                      IsRange = true;
                      End = Some 3uy;},
                     Sequence [Alternatives [Terminals ['2']; Terminals ['3']]])]))
            ]
            |> List.map (fun (str, expected) ->
                testCase (sprintf "basic rule with repetition parsing test: %s" str) <| fun _ ->
                    let res = (run pRule str)
                    Expect.isSuccess res "What is this field for?"
                    Expect.equal "What is this field for?" expected (unwrap res)))

        testList "complex rule parsing test"
           ([
                ("postal-address   = name-part street zip-part", ("postal-address",
                    [RuleReference "name-part"; RuleReference "street"; RuleReference "zip-part"]))
                ("name-part        = *(personal-part SP) last-name [SP suffix] CRLF", ("name-part",
                    [Repetition
                       ({Start = None;
                         IsRange = true;
                         End = None;},
                        Sequence [RuleReference "personal-part"; Alternatives [Terminals [' ']]]);
                     RuleReference "last-name";
                     OptionalSequence
                       (Sequence [Alternatives [Terminals [' ']]; RuleReference "suffix"]);
                     Terminals ['\013'; '\010']]))
                ("name-part        =/ personal-part CRLF", ("name-part", [RuleReference "personal-part"; Terminals ['\013'; '\010']]))
                ("personal-part    = first-name / (initial \".\")", ("personal-part",
                    [Alternatives
                       [RuleReference "first-name";
                        Sequence [RuleReference "initial"; Terminals ['.']]]]))
                ("first-name       = *ALPHA", ("first-name",
                    [Repetition
                       ({Start = None;
                         IsRange = true;
                         End = None;},
                        Alternatives
                          [Terminals ['A']; Terminals ['B']; Terminals ['C']; Terminals ['D'];
                           Terminals ['E']; Terminals ['F']; Terminals ['G']; Terminals ['H'];
                           Terminals ['I']; Terminals ['J']; Terminals ['K']; Terminals ['L'];
                           Terminals ['M']; Terminals ['N']; Terminals ['O']; Terminals ['P'];
                           Terminals ['Q']; Terminals ['R']; Terminals ['S']; Terminals ['T'];
                           Terminals ['U']; Terminals ['V']; Terminals ['W']; Terminals ['X'];
                           Terminals ['Y']; Terminals ['Z']; Terminals ['a']; Terminals ['b'];
                           Terminals ['c']; Terminals ['d']; Terminals ['e']; Terminals ['f'];
                           Terminals ['g']; Terminals ['h']; Terminals ['i']; Terminals ['j'];
                           Terminals ['k']; Terminals ['l']; Terminals ['m']; Terminals ['n'];
                           Terminals ['o']; Terminals ['p']; Terminals ['q']; Terminals ['r'];
                           Terminals ['s']; Terminals ['t']; Terminals ['u']; Terminals ['v'];
                           Terminals ['w']; Terminals ['x']; Terminals ['y']; Terminals ['z']])]))
                ("initial          = ALPHA", ("initial",
                    [Alternatives
                       [Terminals ['A']; Terminals ['B']; Terminals ['C']; Terminals ['D'];
                        Terminals ['E']; Terminals ['F']; Terminals ['G']; Terminals ['H'];
                        Terminals ['I']; Terminals ['J']; Terminals ['K']; Terminals ['L'];
                        Terminals ['M']; Terminals ['N']; Terminals ['O']; Terminals ['P'];
                        Terminals ['Q']; Terminals ['R']; Terminals ['S']; Terminals ['T'];
                        Terminals ['U']; Terminals ['V']; Terminals ['W']; Terminals ['X'];
                        Terminals ['Y']; Terminals ['Z']; Terminals ['a']; Terminals ['b'];
                        Terminals ['c']; Terminals ['d']; Terminals ['e']; Terminals ['f'];
                        Terminals ['g']; Terminals ['h']; Terminals ['i']; Terminals ['j'];
                        Terminals ['k']; Terminals ['l']; Terminals ['m']; Terminals ['n'];
                        Terminals ['o']; Terminals ['p']; Terminals ['q']; Terminals ['r'];
                        Terminals ['s']; Terminals ['t']; Terminals ['u']; Terminals ['v'];
                        Terminals ['w']; Terminals ['x']; Terminals ['y']; Terminals ['z']]]))
                ("last-name        = *ALPHA ; this is a test comment", ("last-name",
                    [Repetition
                       ({Start = None;
                         IsRange = true;
                         End = None;},
                        Alternatives
                          [Terminals ['A']; Terminals ['B']; Terminals ['C']; Terminals ['D'];
                           Terminals ['E']; Terminals ['F']; Terminals ['G']; Terminals ['H'];
                           Terminals ['I']; Terminals ['J']; Terminals ['K']; Terminals ['L'];
                           Terminals ['M']; Terminals ['N']; Terminals ['O']; Terminals ['P'];
                           Terminals ['Q']; Terminals ['R']; Terminals ['S']; Terminals ['T'];
                           Terminals ['U']; Terminals ['V']; Terminals ['W']; Terminals ['X'];
                           Terminals ['Y']; Terminals ['Z']; Terminals ['a']; Terminals ['b'];
                           Terminals ['c']; Terminals ['d']; Terminals ['e']; Terminals ['f'];
                           Terminals ['g']; Terminals ['h']; Terminals ['i']; Terminals ['j'];
                           Terminals ['k']; Terminals ['l']; Terminals ['m']; Terminals ['n'];
                           Terminals ['o']; Terminals ['p']; Terminals ['q']; Terminals ['r'];
                           Terminals ['s']; Terminals ['t']; Terminals ['u']; Terminals ['v'];
                           Terminals ['w']; Terminals ['x']; Terminals ['y']; Terminals ['z']])]))
                ("suffix           = (\"Jr.\" / \"Sr.\" / 1*(\"I\" / \"V\" / \"X\"))", ("suffix",
                    [Sequence
                       [Alternatives
                          [Terminals ['J'; 'r'; '.']; Terminals ['S'; 'r'; '.'];
                           Repetition
                             ({Start = Some 1uy;
                               IsRange = true;
                               End = None;},
                              Sequence
                                [Alternatives [Terminals ['I']; Terminals ['V']; Terminals ['X']]])]]]))
                ("street           = [apt SP] house-num SP street-name CRLF", ("street",
                    [OptionalSequence
                       (Sequence [RuleReference "apt"; Alternatives [Terminals [' ']]]);
                     RuleReference "house-num"; Alternatives [Terminals [' ']];
                     RuleReference "street-name"; Terminals ['\013'; '\010']]))
                ("apt              = 1*4DIGIT", ("apt",
                    [Repetition
                       ({Start = Some 1uy;
                         IsRange = true;
                         End = Some 4uy;},
                        Alternatives
                          [Terminals ['0']; Terminals ['1']; Terminals ['2']; Terminals ['3'];
                           Terminals ['4']; Terminals ['5']; Terminals ['6']; Terminals ['7'];
                           Terminals ['8']; Terminals ['9']])]))
                ("house-num        = 1*8(DIGIT / ALPHA)", ("house-num",
                    [Repetition
                       ({Start = Some 1uy;
                         IsRange = true;
                         End = Some 8uy;},
                        Sequence
                          [Alternatives
                             [Alternatives
                                [Terminals ['0']; Terminals ['1']; Terminals ['2']; Terminals ['3'];
                                 Terminals ['4']; Terminals ['5']; Terminals ['6']; Terminals ['7'];
                                 Terminals ['8']; Terminals ['9']];
                              Alternatives
                                [Terminals ['A']; Terminals ['B']; Terminals ['C']; Terminals ['D'];
                                 Terminals ['E']; Terminals ['F']; Terminals ['G']; Terminals ['H'];
                                 Terminals ['I']; Terminals ['J']; Terminals ['K']; Terminals ['L'];
                                 Terminals ['M']; Terminals ['N']; Terminals ['O']; Terminals ['P'];
                                 Terminals ['Q']; Terminals ['R']; Terminals ['S']; Terminals ['T'];
                                 Terminals ['U']; Terminals ['V']; Terminals ['W']; Terminals ['X'];
                                 Terminals ['Y']; Terminals ['Z']; Terminals ['a']; Terminals ['b'];
                                 Terminals ['c']; Terminals ['d']; Terminals ['e']; Terminals ['f'];
                                 Terminals ['g']; Terminals ['h']; Terminals ['i']; Terminals ['j'];
                                 Terminals ['k']; Terminals ['l']; Terminals ['m']; Terminals ['n'];
                                 Terminals ['o']; Terminals ['p']; Terminals ['q']; Terminals ['r'];
                                 Terminals ['s']; Terminals ['t']; Terminals ['u']; Terminals ['v'];
                                 Terminals ['w']; Terminals ['x']; Terminals ['y']; Terminals ['z']]]])]))
                ("street-name      = 1*VCHAR", ("street-name",
                    [Repetition
                       ({Start = Some 1uy;
                         IsRange = true;
                         End = None;},
                        Alternatives
                          [Terminals ['!']; Terminals ['"']; Terminals ['#']; Terminals ['$'];
                           Terminals ['%']; Terminals ['&']; Terminals ['\'']; Terminals ['('];
                           Terminals [')']; Terminals ['*']; Terminals ['+']; Terminals [','];
                           Terminals ['-']; Terminals ['.']; Terminals ['/']; Terminals ['0'];
                           Terminals ['1']; Terminals ['2']; Terminals ['3']; Terminals ['4'];
                           Terminals ['5']; Terminals ['6']; Terminals ['7']; Terminals ['8'];
                           Terminals ['9']; Terminals [':']; Terminals [';']; Terminals ['<'];
                           Terminals ['=']; Terminals ['>']; Terminals ['?']; Terminals ['@'];
                           Terminals ['A']; Terminals ['B']; Terminals ['C']; Terminals ['D'];
                           Terminals ['E']; Terminals ['F']; Terminals ['G']; Terminals ['H'];
                           Terminals ['I']; Terminals ['J']; Terminals ['K']; Terminals ['L'];
                           Terminals ['M']; Terminals ['N']; Terminals ['O']; Terminals ['P'];
                           Terminals ['Q']; Terminals ['R']; Terminals ['S']; Terminals ['T'];
                           Terminals ['U']; Terminals ['V']; Terminals ['W']; Terminals ['X'];
                           Terminals ['Y']; Terminals ['Z']; Terminals ['[']; Terminals ['\\'];
                           Terminals [']']; Terminals ['^']; Terminals ['_']; Terminals ['`'];
                           Terminals ['a']; Terminals ['b']; Terminals ['c']; Terminals ['d'];
                           Terminals ['e']; Terminals ['f']; Terminals ['g']; Terminals ['h'];
                           Terminals ['i']; Terminals ['j']; Terminals ['k']; Terminals ['l'];
                           Terminals ['m']; Terminals ['n']; Terminals ['o']; Terminals ['p'];
                           Terminals ['q']; Terminals ['r']; Terminals ['s']; Terminals ['t'];
                           Terminals ['u']; Terminals ['v']; Terminals ['w']; Terminals ['x'];
                           Terminals ['y']; Terminals ['z']; Terminals ['{']; Terminals ['|'];
                           Terminals ['}']; Terminals ['~']])]))
                ("zip-part         = town-name \",\" SP state 1*2SP zip-code CRLF", ("zip-part",
                    [RuleReference "town-name"; Terminals [',']; Alternatives [Terminals [' ']];
                     RuleReference "state";
                     Repetition ({Start = Some 1uy;
                                  IsRange = true;
                                  End = Some 2uy;},Alternatives [Terminals [' ']]);
                     RuleReference "zip-code"; Terminals ['\013'; '\010']]))
                ("town-name        = 1*(ALPHA / SP)", ("town-name",
                    [Repetition
                       ({Start = Some 1uy;
                         IsRange = true;
                         End = None;},
                        Sequence
                          [Alternatives
                             [Alternatives
                                [Terminals ['A']; Terminals ['B']; Terminals ['C']; Terminals ['D'];
                                 Terminals ['E']; Terminals ['F']; Terminals ['G']; Terminals ['H'];
                                 Terminals ['I']; Terminals ['J']; Terminals ['K']; Terminals ['L'];
                                 Terminals ['M']; Terminals ['N']; Terminals ['O']; Terminals ['P'];
                                 Terminals ['Q']; Terminals ['R']; Terminals ['S']; Terminals ['T'];
                                 Terminals ['U']; Terminals ['V']; Terminals ['W']; Terminals ['X'];
                                 Terminals ['Y']; Terminals ['Z']; Terminals ['a']; Terminals ['b'];
                                 Terminals ['c']; Terminals ['d']; Terminals ['e']; Terminals ['f'];
                                 Terminals ['g']; Terminals ['h']; Terminals ['i']; Terminals ['j'];
                                 Terminals ['k']; Terminals ['l']; Terminals ['m']; Terminals ['n'];
                                 Terminals ['o']; Terminals ['p']; Terminals ['q']; Terminals ['r'];
                                 Terminals ['s']; Terminals ['t']; Terminals ['u']; Terminals ['v'];
                                 Terminals ['w']; Terminals ['x']; Terminals ['y']; Terminals ['z']];
                              Alternatives [Terminals [' ']]]])]))
                ("state            = 2ALPHA", ("state",
                    [Repetition
                       ({Start = Some 2uy;
                         IsRange = false;
                         End = None;},
                        Alternatives
                          [Terminals ['A']; Terminals ['B']; Terminals ['C']; Terminals ['D'];
                           Terminals ['E']; Terminals ['F']; Terminals ['G']; Terminals ['H'];
                           Terminals ['I']; Terminals ['J']; Terminals ['K']; Terminals ['L'];
                           Terminals ['M']; Terminals ['N']; Terminals ['O']; Terminals ['P'];
                           Terminals ['Q']; Terminals ['R']; Terminals ['S']; Terminals ['T'];
                           Terminals ['U']; Terminals ['V']; Terminals ['W']; Terminals ['X'];
                           Terminals ['Y']; Terminals ['Z']; Terminals ['a']; Terminals ['b'];
                           Terminals ['c']; Terminals ['d']; Terminals ['e']; Terminals ['f'];
                           Terminals ['g']; Terminals ['h']; Terminals ['i']; Terminals ['j'];
                           Terminals ['k']; Terminals ['l']; Terminals ['m']; Terminals ['n'];
                           Terminals ['o']; Terminals ['p']; Terminals ['q']; Terminals ['r'];
                           Terminals ['s']; Terminals ['t']; Terminals ['u']; Terminals ['v'];
                           Terminals ['w']; Terminals ['x']; Terminals ['y']; Terminals ['z']])]))
                ("zip-code         = 5DIGIT [\"-\" 4DIGIT]", ("zip-code",
                    [Repetition
                       ({Start = Some 5uy;
                         IsRange = false;
                         End = None;},
                        Alternatives
                          [Terminals ['0']; Terminals ['1']; Terminals ['2']; Terminals ['3'];
                           Terminals ['4']; Terminals ['5']; Terminals ['6']; Terminals ['7'];
                           Terminals ['8']; Terminals ['9']]);
                     OptionalSequence
                       (Sequence
                          [Terminals ['-'];
                           Repetition
                             ({Start = Some 4uy;
                               IsRange = false;
                               End = None;},
                              Alternatives
                                [Terminals ['0']; Terminals ['1']; Terminals ['2']; Terminals ['3'];
                                 Terminals ['4']; Terminals ['5']; Terminals ['6']; Terminals ['7'];
                                 Terminals ['8']; Terminals ['9']])])]))
            ]
            |> List.map (fun (str, expected) ->
                testCase (sprintf "complex rule parsing test: %s" str) <| fun _ ->
                    let res = (run pRule str)
                    Expect.isSuccess res "What is this field for?"
                    Expect.equal "What is this field for?" expected (unwrap res)))

    ]