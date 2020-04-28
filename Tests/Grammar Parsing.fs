module Tests

open Expecto
open Fab
open Fab.Types

let terminalSingle = TerminalSingle >> Terminals
let terminalGroup = TerminalGroup >> Terminals


let terminalRange = TerminalRange >> Terminals
let terminalHexRange startHex endHex =
    terminalRange ((char startHex), (char endHex))

[<Tests>]
let simple =
    testList "simple parsing tests"
        [ testCase "terminal parsing test"
          <| Helpers.parseAndCompare pTerminal
                 [ (@"%b00010100‬", (Bit, '\020'))
                   (@"%d20", (Digit, '\020'))
                   (@"%x14", (HexDig, '\020')) ]

          testCase "terminals parsing tests"
          <| Helpers.parseAndCompare pTerminals
                 [ (@"%b00010100.00011110.11111111",
                    terminalGroup [ '\020'; '\030'; 'ÿ' ])
                   (@"%d20.30.255",
                    terminalGroup [ '\020'; '\030'; 'ÿ' ])
                   (@"%x14.1E.FF",
                    terminalGroup [ '\020'; '\030'; 'ÿ' ]) ]

          testCase "terminal range parsing tests"
          <| Helpers.parseAndCompare pTerminals
                 [ (@"%b0000000-11111111",
                    terminalRange ('\000', '\255'))
                   (@"%d0-255",
                    terminalRange ('\000', '\255'))
                   (@"%x0-FF",
                    terminalRange ('\000', '\255')) ]

          testCase "string parsing tests"
          <| Helpers.parseAndCompare pString
                 [ (@"""this is a string""",
                    REString "this is a string")
                   (@"""this""", REString "this")
                   (@"""thisa235hfnv""",
                    REString "thisa235hfnv")
                   (@"""SP DIGIT ALPHA""",
                    REString "SP DIGIT ALPHA")

                   (@"""(THIS SHOULD NOT GET PARSED AS ANYTHING OTHER THAN A STRING )""",
                    REString
                        "(THIS SHOULD NOT GET PARSED AS ANYTHING OTHER THAN A STRING )")

                   (@"""!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~""",
                    REString
                        """!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~""") ]

          testCase "repetition parsing tests"
          <| Helpers.parseAndCompare pRange
                 [ (@"2", Exactly 2uy)
                   (@"*4", AtMost 4uy)
                   (@"2*4", Between(2uy, 4uy))
                   (@"*", Any) ]

          testCase "core rule parsing tests"
          <| Helpers.parseAndCompare pCoreRule
                 [ ("ALPHA", CoreRule ALPHA)
                   ("DIGIT", CoreRule DIGIT)
                   ("HEXDIG", CoreRule HEXDIG)
                   ("DQUOTE", CoreRule DQUOTE)
                   ("SP", CoreRule SP)
                   ("HTAB", CoreRule HTAB)
                   ("WSP", CoreRule WSP)
                   ("VCHAR", CoreRule VCHAR)
                   ("CHAR", CoreRule CHAR)
                   ("OCTET", CoreRule OCTET)
                   ("CTL", CoreRule CTL)
                   ("CR", CoreRule CR)
                   ("LF", CoreRule LF)
                   ("CRLF", CoreRule CRLF)
                   ("BIT", CoreRule BIT) ] ]

[<Tests>]
let groups =
    testList "group parsing tests"
        [ testCase "sequence parsing tests"
          <| Helpers.parseAndCompare pAlternates
                 [ (@"%d1", terminalSingle '\001')

                   (@"%d1 %d2",
                    Sequence
                        [ terminalSingle '\001'
                          terminalSingle '\002' ])

                   (@"%d1 %d2 / %d3",
                    Alternatives
                        [ Sequence
                            [ terminalSingle '\001'

                              terminalSingle '\002' ]
                          terminalSingle '\003' ])
                   (@"""1"" ""2"" ""3""",
                    Sequence
                        [ REString "1"
                          REString "2"
                          REString "3" ]) ]

          testCase "sequence group parsing tests"
          <| Helpers.parseAndCompare pSequenceGroup
                 [ (@"(%d1)", terminalSingle '\001')
                   (@"(%d1 %d2)",
                    Sequence
                        [ terminalSingle '\001'
                          terminalSingle '\002' ])
                   (@"(%d1 %d2 / %d3)",
                    Alternatives
                        [ Sequence
                            [ terminalSingle '\001'
                              terminalSingle '\002' ]
                          terminalSingle '\003' ])
                   (@"(""1"" ""2"" ""3"")",
                    Sequence
                        [ REString "1"
                          REString "2"
                          REString "3" ]) ]

          testCase "alternate parsing tests"
          <| Helpers.parseAndCompare pAlternates
                 [ (@"%d1", terminalSingle '\001')
                   (@"%d1 / %d2",
                    Alternatives
                        [ terminalSingle '\001'
                          terminalSingle '\002' ])
                   (@"%d1 / %d2 / %d3",
                    Alternatives
                        [ terminalSingle '\001'
                          terminalSingle '\002'
                          terminalSingle '\003' ])
                   (@"""1"" / ""2"" / ""3""",
                    Alternatives
                        [ REString "1"
                          REString "2"
                          REString "3" ]) ]

          testCase "optional group parsing tests"
          <| Helpers.parseAndCompare pOptionalGroup
                 [ (@"[%d1]",
                    OptionalSequence(terminalSingle '\001'))
                   (@"[%d1 %d2]",
                    OptionalSequence
                        (Sequence
                            [ terminalSingle '\001'
                              terminalSingle '\002' ]))
                   (@"[%d1 %d2 %d3]",
                    OptionalSequence
                        (Sequence
                            [ terminalSingle '\001'
                              terminalSingle '\002'
                              terminalSingle '\003' ]))
                   (@"[""1"" ""2"" ""3""]",
                    OptionalSequence
                        (Sequence
                            [ REString "1"
                              REString "2"
                              REString "3" ])) ]

          testCase "groups containing rule references tests"
          <| Helpers.parseAndCompare pSequenceGroup
                 [ ("(rule-ref another-rule-ref DIGIT)",
                    Sequence
                        [ RuleReference "rule-ref"
                          RuleReference "another-rule-ref"
                          CoreRule DIGIT ]) ] ]

[<Tests>]
let combinations =
    testList "combination parsing tests"
        [ testCase "group combination parsing test"
          <| Helpers.parseAndCompare pAlternates
                 [ (@"""0"" / (""1"" / ""2"") / ""3""",
                    Alternatives
                        [ REString "0"
                          Alternatives
                              [ REString "1"
                                REString "2" ]
                          REString "3" ])
                   (@"[""0"" / (""1"" / ""2"")] / ""3"" ""4""",
                    Alternatives
                        [ OptionalSequence
                            (Alternatives
                                [ REString "0"
                                  Alternatives
                                      [ REString "1"
                                        REString "2" ] ])
                          Sequence
                              [ REString "3"
                                REString "4" ] ])
                   (@"""0"" / [""1"" / ""2""] / ""3""",
                    Alternatives
                        [ REString "0"

                          OptionalSequence
                              (Alternatives
                                  [ REString "1"
                                    REString "2" ])
                          REString "3" ])
                   (@"%x20 / (""1"" / ""2"") / %x20",
                    Alternatives
                        [ terminalSingle ' '
                          Alternatives
                              [ REString "1"
                                REString "2" ]
                          terminalSingle ' ' ])
                   (@"[%d20 / (""1"" / ""2"")] / %b0101",
                    Alternatives
                        [ OptionalSequence
                            (Alternatives
                                [ terminalSingle '\020'
                                  Alternatives
                                      [ REString "1"
                                        REString "2" ] ])
                          terminalSingle '\005' ])
                   (@"%b0101 / [""1"" / ""2""] / %x20",
                    Alternatives
                        [ terminalSingle '\005'

                          OptionalSequence
                              (Alternatives
                                  [ REString "1"
                                    REString "2" ])
                          terminalSingle ' ' ])
                   (@"[%b0101 / [""1"" / ""2""] / %x20]",
                    OptionalSequence
                        (Alternatives
                            [ terminalSingle '\005'

                              OptionalSequence
                                  (Alternatives
                                      [ REString "1"
                                        REString "2" ])
                              terminalSingle ' ' ]))
                   (@"[%b0101 / (%x0F) / %x20]",
                    OptionalSequence
                        (Alternatives
                            [ terminalSingle '\005'
                              terminalSingle '\015'
                              terminalSingle ' ' ])) ]

          testCase "group repetition parsing test"
          <| Helpers.parseAndCompare pAlternates
                 [ (@"1*2test-rule",
                    Repetition
                        (Between(1uy, 2uy),
                         RuleReference "test-rule"))
                   (@"1*2""2""",
                    Repetition
                        (Between(1uy, 2uy), REString "2"))
                   (@"1*2(""2"" / ""3"")",
                    Repetition
                        (Between(1uy, 2uy),
                         Alternatives
                             [ REString "2"
                               REString "3" ]))
                   (@"1*2[""2"" / ""3""]",
                    Repetition
                        (Between(1uy, 2uy),
                         OptionalSequence
                             (Alternatives
                                 [ REString "2"
                                   REString "3" ])))
                   (@"*2""2""",
                    Repetition(AtMost 2uy, REString "2"))
                   (@"*2(""2"" / ""3"")",
                    Repetition
                        (AtMost 2uy,
                         Alternatives
                             [ REString "2"
                               REString "3" ]))
                   (@"*2[""2"" / ""3""]",
                    Repetition
                        (AtMost 2uy,
                         OptionalSequence
                             (Alternatives
                                 [ REString "2"
                                   REString "3" ])))
                   (@"2*""2""",
                    Repetition(AtLeast 2uy, REString "2"))
                   (@"2*(""2"" / ""3"")",
                    Repetition
                        (AtLeast 2uy,
                         Alternatives
                             [ REString "2"
                               REString "3" ]))
                   (@"2*[""2"" / ""3""]",
                    Repetition
                        (AtLeast 2uy,
                         OptionalSequence
                             (Alternatives
                                 [ REString "2"
                                   REString "3" ])))
                   (@"*""2""", Repetition(Any, REString "2"))
                   (@"*(""2"" / ""3"")",
                    Repetition
                        (Any,
                         Alternatives
                             [ REString "2"
                               REString "3" ]))
                   (@"*[""2"" / ""3""]",
                    Repetition
                        (Any,
                         OptionalSequence
                             (Alternatives
                                 [ REString "2"
                                   REString "3" ])))
                   (@"*(WSP / CRLF WSP)",
                    Repetition
                        (Any,
                         Alternatives
                             [ CoreRule WSP
                               Sequence
                                   [ CoreRule CRLF
                                     CoreRule WSP ] ])) ]

          testCase
              "multiple sequential repetition parsing test"
          <| Helpers.parseAndCompare pAlternates
                 [ (@"1*2""2"" 2*3""3""",
                    Sequence
                        [ Repetition
                            (Between(1uy, 2uy), REString "2")
                          Repetition
                              (Between(2uy, 3uy),
                               REString "3") ])
                   (@"1*2(ALPHA DIGIT) *(DIGIT ALPHA)",
                    Sequence
                        [ Repetition
                            (Between(1uy, 2uy),
                             Sequence
                                 [ CoreRule ALPHA
                                   CoreRule DIGIT ])
                          Repetition
                              (Any,
                               Sequence
                                   [ CoreRule DIGIT
                                     CoreRule ALPHA ]) ]) ] ]

[<Tests>]
let rules =
    testList "rule parsing tests"
        [ testCase "basic rule parsing test"
          <| Helpers.parseAndCompare pRule
                 [ (@"TEST = ""0"" ""1"" ""2"" ""3"" ""4"" ""5"" ""6"" ""7""",
                    ("TEST",
                     [ Sequence
                         [ REString "0"
                           REString "1"
                           REString "2"
                           REString "3"
                           REString "4"
                           REString "5"
                           REString "6"
                           REString "7" ] ]))

                   (@"TEST = ""0"" / ""1"" / ""2"" / ""3"" / ""4"" / ""5"" / ""6"" / ""7""",
                    ("TEST",
                     [ Alternatives
                         [ REString "0"
                           REString "1"
                           REString "2"
                           REString "3"
                           REString "4"
                           REString "5"
                           REString "6"
                           REString "7" ] ]))

                   (@"TEST = ""0""",
                    ("TEST", [ REString "0" ]))

                   (@"TEST = ""0"" / (""1"" ""2"") / ""3"" / (""4"" / ""5"") / ""6"" / ""7""",
                    ("TEST",
                     [ Alternatives
                         [ REString "0"

                           Sequence
                               [ REString "1"

                                 REString "2" ]
                           REString "3"

                           Alternatives
                               [ REString "4"

                                 REString "5" ]
                           REString "6"
                           REString "7" ] ]))
                   (@"TEST = ""0"" (""1"" / ""2"") ""3"" (""4"" / ""5"") ""6"" ""7""",
                    ("TEST",
                     [ Sequence
                         [ REString "0"

                           Alternatives
                               [ REString "1"
                                 REString "2" ]
                           REString "3"

                           Alternatives
                               [ REString "4"
                                 REString "5" ]
                           REString "6"
                           REString "7" ] ]))
                   (@"TEST = (""1"" / ""2"") ""3"" (""4"" / ""5"") ""6"" ""7""",
                    ("TEST",
                     [ Sequence
                         [ Alternatives
                             [ REString "1"
                               REString "2" ]
                           REString "3"

                           Alternatives
                               [ REString "4"
                                 REString "5" ]
                           REString "6"
                           REString "7" ] ])) ]

          testCase "basic rule with repetition parsing test"
          <| Helpers.parseAndCompare pRule
                 [ (@"test-rule0 = 1*2""2"" 2*3""3""",
                    ("test-rule0",
                     [ Sequence
                         [ Repetition
                             (Between(1uy, 2uy),
                              REString "2")
                           Repetition
                               (Between(2uy, 3uy),
                                REString "3") ] ]))

                   (@"test-rule1 = 1*2""2"" 2*3test-reference",
                    ("test-rule1",
                     [ Sequence
                         [ Repetition
                             (Between(1uy, 2uy),
                              REString "2")

                           Repetition
                               (Between(2uy, 3uy),
                                RuleReference
                                    "test-reference") ] ]))

                   (@"test-rule2 = 1*2(""2"" / test-reference) 2*3[""2"" / test-reference]",
                    ("test-rule2",
                     [ Sequence
                         [ Repetition
                             (Between(1uy, 2uy),
                              Alternatives
                                  [ REString "2"
                                    RuleReference
                                        "test-reference" ])

                           Repetition
                               (Between(2uy, 3uy),
                                OptionalSequence
                                    (Alternatives
                                        [ REString "2"
                                          RuleReference
                                              "test-reference" ])) ] ]))

                   (@"test-rule3 = 1*2(""2"" / ""3"") 2*3[""2"" / ""3""]",
                    ("test-rule3",
                     [ Sequence
                         [ Repetition
                             (Between(1uy, 2uy),
                              Alternatives
                                  [ REString "2"
                                    REString "3" ])

                           Repetition
                               (Between(2uy, 3uy),
                                OptionalSequence
                                    (Alternatives
                                        [ REString "2"
                                          REString "3" ])) ] ]))

                   (@"test-rule4 = 1*2[""2"" / ""3""] 2*3(""2"" / ""3"")",
                    ("test-rule4",
                     [ Sequence
                         [ Repetition
                             (Between(1uy, 2uy),
                              OptionalSequence
                                  (Alternatives
                                      [ REString "2"
                                        REString "3" ]))
                           Repetition
                               (Between(2uy, 3uy),
                                Alternatives
                                    [ REString "2"
                                      REString "3" ]) ] ])) ]

          testCase
              "complex rule parsing test - postal address"
          <| Helpers.parseAndCompare pRule
                 [ (@"postal-address   = name-part street zip-part",
                    ("postal-address",
                     [ Sequence
                         [ RuleReference "name-part"
                           RuleReference "street"
                           RuleReference "zip-part" ] ]))
                   (@"name-part        = *(personal-part SP) last-name [SP suffix] CRLF",
                    ("name-part",
                     [ Sequence
                         [ Repetition
                             (Any,
                              Sequence
                                  [ RuleReference
                                      "personal-part"
                                    CoreRule SP ])
                           RuleReference "last-name"

                           OptionalSequence
                               (Sequence
                                   [ CoreRule SP
                                     RuleReference "suffix" ])
                           CoreRule CRLF ] ]))
                   (@"name-part        = personal-part CRLF",
                    ("name-part",
                     [ Sequence
                         [ RuleReference "personal-part"
                           CoreRule CRLF ] ]))
                   (@"personal-part    = first-name / (initial ""."")",
                    ("personal-part",
                     [ Alternatives
                         [ RuleReference "first-name"
                           Sequence
                               [ RuleReference "initial"
                                 REString "." ] ] ]))

                   (@"first-name       = *ALPHA",
                    ("first-name",
                     [ Repetition(Any, CoreRule ALPHA) ]))

                   (@"initial          = ALPHA",
                    ("initial", [ CoreRule ALPHA ]))

                   (@"last-name        = *ALPHA ; this is a test comment",
                    ("last-name",
                     [ Repetition(Any, CoreRule ALPHA) ]))

                   (@"suffix           = (""Jr."" / ""Sr."" / 1*(""I"" / ""V"" / ""X""))",
                    ("suffix",
                     [ Alternatives
                         [ REString "Jr."
                           REString "Sr."
                           Repetition
                               (AtLeast 1uy,
                                Alternatives
                                    [ REString "I"
                                      REString "V"
                                      REString "X" ]) ] ]))
                   (@"street           = [apt SP] house-num SP street-name CRLF",
                    ("street",
                     [ Sequence
                         [ OptionalSequence
                             (Sequence
                                 [ RuleReference "apt"
                                   CoreRule SP ])
                           RuleReference "house-num"
                           CoreRule SP
                           RuleReference "street-name"
                           CoreRule CRLF ] ]))

                   (@"apt              = 1*4DIGIT",
                    ("apt",
                     [ Repetition
                         (Between(1uy, 4uy), CoreRule DIGIT) ]))
                   (@"house-num        = 1*8(DIGIT / ALPHA)",
                    ("house-num",
                     [ Repetition
                         (Between(1uy, 8uy),
                          Alternatives
                              [ CoreRule DIGIT
                                CoreRule ALPHA ]) ]))

                   (@"street-name      = 1*VCHAR",
                    ("street-name",
                     [ Repetition
                         (AtLeast 1uy, CoreRule VCHAR) ]))
                   (@"zip-part         = town-name "","" SP state 1*2SP zip-code CRLF",
                    ("zip-part",
                     [ Sequence
                         [ RuleReference "town-name"
                           REString ","
                           CoreRule SP
                           RuleReference "state"
                           Repetition
                               (Between(1uy, 2uy),
                                CoreRule SP)
                           RuleReference "zip-code"
                           CoreRule CRLF ] ]))
                   (@"town-name        = 1*(ALPHA / SP)",
                    ("town-name",
                     [ Repetition
                         (AtLeast 1uy,
                          Alternatives
                              [ CoreRule ALPHA
                                CoreRule SP ]) ]))

                   (@"state            = 2ALPHA",
                    ("state",
                     [ Repetition
                         (Exactly 2uy, CoreRule ALPHA) ]))

                   (@"zip-code         = 5DIGIT [""-"" 4DIGIT]",
                    ("zip-code",
                     [ Sequence
                         [ Repetition
                             (Exactly 5uy, CoreRule DIGIT)

                           OptionalSequence
                               (Sequence
                                   [ REString "-"
                                     Repetition
                                         (Exactly 4uy,
                                          CoreRule DIGIT) ]) ] ])) ]

          testCase "complex rule parsing test - IRC"
          <| Helpers.parseAndCompare pRule
                 [ (@"message    =  [ "":"" prefix SPACE ] command [ params ] crlf",
                    ("message",
                     [ Sequence
                         [ OptionalSequence
                             (Sequence
                                 [ REString ":"
                                   RuleReference "prefix"
                                   RuleReference "SPACE" ])
                           RuleReference "command"
                           OptionalSequence
                               (RuleReference "params")
                           RuleReference "crlf" ] ]))

                   (@"prefix     =  servername / ( nickname [ [ ""!"" user ] ""@"" host ] )",
                    ("prefix",
                     [ Alternatives
                         [ RuleReference "servername"

                           Sequence
                               [ RuleReference "nickname"

                                 OptionalSequence
                                     (Sequence
                                         [ OptionalSequence
                                             (Sequence
                                                 [ REString
                                                     "!"

                                                   RuleReference
                                                       "user" ])
                                           REString "@"
                                           RuleReference
                                               "host" ]) ] ] ]))

                   (@"command    =  1*letter / 3digit",
                    ("command",
                     [ Alternatives
                         [ Repetition
                             (AtLeast 1uy,
                              RuleReference "letter")
                           Repetition
                               (Exactly 3uy,
                                RuleReference "digit") ] ]))

                   (@"params     =  *14( SPACE middle ) [ SPACE "":"" trailing ]",
                    ("params",
                     [ Sequence
                         [ Repetition
                             (AtMost 14uy,
                              Sequence
                                  [ RuleReference "SPACE"
                                    RuleReference "middle" ])
                           OptionalSequence
                               (Sequence
                                   [ RuleReference "SPACE"
                                     REString ":"
                                     RuleReference
                                         "trailing" ]) ] ]))

                   (@"params     = 14( SPACE middle ) [ SPACE [ "":"" ] trailing ]",
                    ("params",
                     [ Sequence
                         [ Repetition
                             (Exactly 14uy,
                              Sequence
                                  [ RuleReference "SPACE"
                                    RuleReference "middle" ])
                           OptionalSequence
                               (Sequence
                                   [ RuleReference "SPACE"

                                     OptionalSequence
                                         (REString ":")
                                     RuleReference
                                         "trailing" ]) ] ]))
                   (@"nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF",
                    ("nospcrlfcl",
                     [ Alternatives
                         [ terminalHexRange 0x01 0x09
                           terminalHexRange 0x0B 0x0C
                           terminalHexRange 0x0E 0x1F
                           terminalHexRange 0x21 0x39
                           terminalHexRange 0x3B 0xFF ] ]))

                   (@"middle     =  nospcrlfcl *( "":"" / nospcrlfcl )",
                    ("middle",
                     [ Sequence
                         [ RuleReference "nospcrlfcl"

                           Repetition
                               (Any,
                                Alternatives
                                    [ REString ":"
                                      RuleReference
                                          "nospcrlfcl" ]) ] ]))

                   (@"trailing   =  *( "":"" / "" "" / nospcrlfcl )",
                    ("trailing",
                     [ Repetition
                         (Any,
                          Alternatives
                              [ REString ":"
                                REString " "
                                RuleReference "nospcrlfcl" ]) ]))

                   (@"SPACE      =  %x20        ; space character",
                    ("SPACE", [ terminalSingle ' ' ]))
                   (@"crlf       =  %x0D %x0A   ; ""carriage return"" ""linefeed""",
                    ("crlf",
                     [ Sequence
                         [ terminalSingle '\013'
                           terminalSingle '\010' ] ]))
                   (@"target     =  nickname / server",
                    ("target",
                     [ Alternatives
                         [ RuleReference "nickname"
                           RuleReference "server" ] ]))
                   (@"msgtarget  =  msgto *( "","" msgto )",
                    ("msgtarget",
                     [ Sequence
                         [ RuleReference "msgto"
                           Repetition
                               (Any,
                                Sequence
                                    [ REString ","
                                      RuleReference "msgto" ]) ] ]))

                   (@"msgto      =  channel / ( user [ ""%"" host ] ""@"" servername )",
                    ("msgto",
                     [ Alternatives
                         [ RuleReference "channel"
                           Sequence
                               [ RuleReference "user"

                                 OptionalSequence
                                     (Sequence
                                         [ REString "%"
                                           RuleReference
                                               "host" ])
                                 REString "@"
                                 RuleReference "servername" ] ] ]))
                   (@"msgto      = ( user ""%"" host ) / targetmask",
                    ("msgto",
                     [ Alternatives
                         [ Sequence
                             [ RuleReference "user"
                               REString "%"
                               RuleReference "host" ]
                           RuleReference "targetmask" ] ]))
                   (@"msgto      = nickname / ( nickname ""!"" user ""@"" host )",
                    ("msgto",
                     [ Alternatives
                         [ RuleReference "nickname"
                           Sequence
                               [ RuleReference "nickname"
                                 REString "!"
                                 RuleReference "user"
                                 REString "@"
                                 RuleReference "host" ] ] ]))

                   (@"channel    =  ( ""#"" / ""+"" / ( ""!"" channelid ) / ""&"" ) chanstring [ "":"" chanstring ]",
                    ("channel",
                     [ Sequence
                         [ Alternatives
                             [ REString "#"
                               REString "+"

                               Sequence
                                   [ REString "!"
                                     RuleReference
                                         "channelid" ]
                               REString "&" ]
                           RuleReference "chanstring"
                           OptionalSequence
                               (Sequence
                                   [ REString ":"
                                     RuleReference
                                         "chanstring" ]) ] ]))

                   (@"servername =  hostname",
                    ("servername",
                     [ RuleReference "hostname" ]))
                   (@"host       =  hostname / hostaddr",
                    ("host",
                     [ Alternatives
                         [ RuleReference "hostname"
                           RuleReference "hostaddr" ] ]))
                   (@"hostname   =  shortname *( ""."" shortname )",
                    ("hostname",
                     [ Sequence
                         [ RuleReference "shortname"
                           Repetition
                               (Any,
                                Sequence
                                    [ REString "."
                                      RuleReference
                                          "shortname" ]) ] ]))

                   (@"shortname  =  ( letter / digit ) *( letter / digit / ""-"" ) *( letter / digit )",
                    ("shortname",
                     [ Sequence
                         [ Alternatives
                             [ RuleReference "letter"
                               RuleReference "digit" ]
                           Repetition
                               (Any,
                                Alternatives
                                    [ RuleReference "letter"

                                      RuleReference "digit"
                                      REString "-" ])

                           Repetition
                               (Any,
                                Alternatives
                                    [ RuleReference "letter"
                                      RuleReference "digit" ]) ] ]))
                   (@"hostaddr   =  ip4addr / ip6addr",
                    ("hostaddr",
                     [ Alternatives
                         [ RuleReference "ip4addr"
                           RuleReference "ip6addr" ] ]))
                   (@"ip4addr    =  1*3digit ""."" 1*3digit ""."" 1*3digit ""."" 1*3digit",
                    ("ip4addr",
                     [ Sequence
                         [ Repetition
                             (Between(1uy, 3uy),
                              RuleReference "digit")
                           REString "."
                           Repetition
                               (Between(1uy, 3uy),
                                RuleReference "digit")
                           REString "."
                           Repetition
                               (Between(1uy, 3uy),
                                RuleReference "digit")
                           REString "."
                           Repetition
                               (Between(1uy, 3uy),
                                RuleReference "digit") ] ]))

                   (@"ip6addr    =  1*hexdigit 7( "":"" 1*hexdigit )",
                    ("ip6addr",
                     [ Sequence
                         [ Repetition
                             (AtLeast 1uy,
                              RuleReference "hexdigit")

                           Repetition
                               (Exactly 7uy,
                                Sequence
                                    [ REString ":"

                                      Repetition
                                          (AtLeast 1uy,
                                           RuleReference
                                               "hexdigit") ]) ] ]))
                   (@"ip6addr    = ""0:0:0:0:0:"" ( ""0"" / ""FFFF"" ) "":"" ip4addr",
                    ("ip6addr",
                     [ Sequence
                         [ REString "0:0:0:0:0:"
                           Alternatives
                               [ REString "0"
                                 REString "FFFF" ]
                           REString ":"
                           RuleReference "ip4addr" ] ]))

                   (@"nickname   =  ( letter / special ) *8( letter / digit / special / ""-"" )",
                    ("nickname",
                     [ Sequence
                         [ Alternatives
                             [ RuleReference "letter"
                               RuleReference "special" ]
                           Repetition
                               (AtMost 8uy,
                                Alternatives
                                    [ RuleReference "letter"

                                      RuleReference "digit"

                                      RuleReference
                                          "special"
                                      REString "-" ]) ] ]))
                   (@"targetmask =  ( ""$"" / ""#"" ) mask",
                    ("targetmask",
                     [ Sequence
                         [ Alternatives
                             [ REString "$"
                               REString "#" ]
                           RuleReference "mask" ] ]))

                   (@"chanstring =  %x01-07 / %x08-09 / %x0B-0C / %x0E-1F / %x21-2B",
                    ("chanstring",
                     [ Alternatives
                         [ terminalHexRange 0x01 0x07
                           terminalHexRange 0x08 0x09
                           terminalHexRange 0x0B 0x0C
                           terminalHexRange 0x0E 0x1F
                           terminalHexRange 0x21 0x2B ] ]))
                   (@"chanstring = %x2D-39 / %x3B-FF",
                    ("chanstring",
                     [ Alternatives
                         [ terminalHexRange 0x2D 0x39
                           terminalHexRange 0x3B 0xFF ] ]))

                   (@"channelid  = 5( %x41-5A / digit )   ",
                    ("channelid",
                     [ Repetition
                         (Exactly 5uy,
                          Alternatives
                              [ terminalHexRange 0x41 0x5A
                                RuleReference "digit" ]) ]))

                   (@"user       =  1*( %x01-09 / %x0B-0C / %x0E-1F / %x21-3F / %x41-FF )",
                    ("user",
                     [ Repetition
                         (AtLeast 1uy,
                          Alternatives
                              [ terminalHexRange 0x01 0x09
                                terminalHexRange 0x0B 0x0C
                                terminalHexRange 0x0E 0x1F
                                terminalHexRange 0x21 0x3F
                                terminalHexRange 0x41 0xFF ]) ]))

                   (@"key        =  1*23( %x01-05 / %x07-08 / %x0C / %x0E-1F / %x21-7F )",
                    ("key",
                     [ Repetition
                         (Between(1uy, 23uy),
                          Alternatives
                              [ terminalHexRange 0x01 0x05
                                terminalHexRange 0x07 0x08
                                terminalSingle (char 0x0C)
                                terminalHexRange 0x0E 0x1F
                                terminalHexRange 0x21 0x7F ]) ]))

                   (@"letter     =  %x41-5A / %x61-7A",
                    ("letter",
                     [ Alternatives
                         [ terminalHexRange 0x41 0x5A
                           terminalHexRange 0x61 0x7A ] ]))

                   (@"digit      =  %x30-39          ",
                    ("digit",
                     [ terminalHexRange 0x30 0x39 ]))
                   (@"hexdigit   =  digit / ""A"" / ""B"" / ""C"" / ""D"" / ""E"" / ""F""",
                    ("hexdigit",
                     [ Alternatives
                         [ RuleReference "digit"
                           REString "A"
                           REString "B"
                           REString "C"
                           REString "D"
                           REString "E"
                           REString "F" ] ]))
                   (@"special    =  %x5B-60 / %x7B-7D",
                    ("special",
                     [ Alternatives
                         [ terminalHexRange 0x5B 0x60
                           terminalHexRange 0x7B 0x7D ] ]))
                   (@"mask       =  *( nowild / noesc wildone / noesc wildmany )",
                    ("mask",
                     [ Repetition
                         (Any,
                          Alternatives
                              [ RuleReference "nowild"
                                Sequence
                                    [ RuleReference "noesc"
                                      RuleReference
                                          "wildone" ]
                                Sequence
                                    [ RuleReference "noesc"
                                      RuleReference
                                          "wildmany" ] ]) ]))
                   (@"wildone    =  %x3F",
                    ("wildone",
                     [ terminalSingle (char 0x3F) ]))
                   (@"wildmany   =  %x2A",
                    ("wildmany",
                     [ terminalSingle (char 0x2A) ]))
                   (@"nowild     =  %x01-29 / %x2B-3E / %x40-FF  ; any octet except NUL, ""*"", ""?""",
                    ("nowild",
                     [ Alternatives
                         [ terminalHexRange 0x01 0x29
                           terminalHexRange 0x2B 0x3E
                           terminalHexRange 0x40 0xFF ] ]))
                   (@"noesc      =  %x01-5B / %x5D-FF  ; any octet except NUL and ""\""",
                    ("noesc",
                     [ Alternatives
                         [ terminalHexRange 0x01 0x5B
                           terminalHexRange 0x5D 0xFF ] ]))
                   (@"matchone   =  %x01-FF    ; matches wildone",
                    ("matchone",
                     [ terminalHexRange 0x01 0xFF ]))

                   (@"matchmany  =  *matchone    ; matches wildmany",
                    ("matchmany",
                     [ Repetition
                         (Any, RuleReference "matchone") ])) ] ]

[<Tests>]
let documentParsing =
    testList "document parsing tests"
        [ testCase "IRC document" (fun _ ->
              let document = """
; Pulled from https://tools.ietf.org/html/rfc2812#section-3
; With some Errata applied:
; * https://www.rfc-editor.org/errata/eid4289
; * https://www.rfc-editor.org/errata/eid3783
; * https://www.rfc-editor.org/errata/eid4836

;   The extracted message is parsed into the components <prefix>,
;   <command> and list of parameters (<params>).
;
;    The Augmented BNF representation for this is:

message    =  [ ":" prefix SPACE ] command [ params ] crlf
prefix     =  servername / ( nickname [ [ "!" user ] "@" host ] )
command    =  1*letter / 3digit
params     =  *14( SPACE middle ) [ SPACE ":" trailing ]
params     =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]

nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
                ; any octet except NUL, CR, LF, " " and ":"
middle     =  nospcrlfcl *( ":" / nospcrlfcl )
trailing   =  *( ":" / " " / nospcrlfcl )

SPACE      =  %x20        ; space character
crlf       =  %x0D %x0A   ; "carriage return" "linefeed"


; Most protocol messages specify additional semantics and syntax for
;   the extracted parameter strings dictated by their position in the
;   list.  For example, many server commands will assume that the first
;   parameter after the command is the list of targets, which can be
;   described with:
target     =  nickname / servername
msgtarget  =  msgto *( "," msgto )
msgto      =  channel / ( user [ "%" host ] "@" servername )
msgto      =/ ( user "%" host ) / targetmask
msgto      =/ nickname / ( nickname "!" user "@" host )
channel    =  ( "#" / "+" / ( "!" channelid ) / "&" ) chanstring [ ":" chanstring ]
servername =  hostname
host       =  hostname / hostaddr
hostname   =  shortname *( "." shortname )
shortname  =  ( letter / digit ) *( letter / digit / "-" ) *( letter / digit )
                ; as specified in RFC 1123 [HNAME]
hostaddr   =  ip4addr / ip6addr
ip4addr    =  1*3digit "." 1*3digit "." 1*3digit "." 1*3digit
ip6addr    =  1*hexdigit 7( ":" 1*hexdigit )
ip6addr    =/ "0:0:0:0:0:" ( "0" / "FFFF" ) ":" ip4addr
nickname   =  ( letter / special ) *8( letter / digit / special / "-" )
targetmask =  ( "$" / "#" ) mask
                ; see details on allowed masks in section 3.3.1
chanstring = *49(%x01-06 / %x08-09 / %x0B-0C / %x0E-1F / %x21-2B / %x2D-39 / %x3B-FF)
                ; any octet except NUL, BELL, CR, LF, " ", "," and ":"
channelid  = 5( %x41-5A / digit )   ; 5( A-Z / 0-9 )

; Other parameter syntaxes are:
user       =  1*( %x01-09 / %x0B-0C / %x0E-1F / %x21-3F / %x41-FF )
                ; any octet except NUL, CR, LF, " " and "@"
key        =  1*23( %x01-08 / %x0E-1F / %x21-7F )
                ; any 7-bit US_ASCII character,
                ; except NUL, CR, LF, FF, h/v TABs, and " "
letter     =  %x41-5A / %x61-7A       ; A-Z / a-z
digit      =  %x30-39                 ; 0-9
hexdigit   =  digit / "A" / "B" / "C" / "D" / "E" / "F"
special    =  %x5B-60 / %x7B-7D
                ; "[", "]", "\", "`", "_", "^", "{", "|", "}"


; 2.5 Wildcard expressions
;
;    When wildcards are allowed in a string, it is referred as a "mask".
;
;    For string matching purposes, the protocol allows the use of two
;    special characters: '?' (%x3F) to match one and only one character,
;    and '*' (%x2A) to match any number of any characters.  These two
;    characters can be escaped using the character '\' (%x5C).
;
;    The Augmented BNF syntax for this is:
mask       =  *( nowild / noesc wildone / noesc wildmany )
wildone    =  %x3F
wildmany   =  %x2A
nowild     =  %x01-29 / %x2B-3E / %x40-FF
                ; any octet except NUL, "*", "?"
noesc      =  %x01-5B / %x5D-FF
                ; any octet except NUL and "\"
matchone   =  %x01-FF
                ; matches wildone
matchmany  =  *matchone
                ; matches wildmany
"""
              Helpers.run pDocument document
              |> Helpers.unwrap
              |> ignore) ]
