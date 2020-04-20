module Tests

open Expecto
open Grammar

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
                    Terminals [ '\020'; '\030'; 'ÿ' ])
                   (@"%d20.30.255",
                    Terminals [ '\020'; '\030'; 'ÿ' ])
                   (@"%x14.1E.FF",
                    Terminals [ '\020'; '\030'; 'ÿ' ]) ]

          testCase "terminal range parsing tests"
          <| Helpers.parseAndCompare pTerminals
                 [ (@"%b0000000-11111111",
                    Terminals [ '\000' .. '\255' ])
                   (@"%d0-255",
                    Terminals [ '\000' .. '\255' ])
                   (@"%x0-FF",
                    Terminals [ '\000' .. '\255' ]) ]

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
                 [ (@"%d1", Terminals [ '\001' ])

                   (@"%d1 %d2",
                    Sequence
                        [ Terminals [ '\001' ]
                          Terminals [ '\002' ] ])

                   (@"%d1 %d2 / %d3",
                    Alternatives
                        [ Sequence
                            [ Terminals [ '\001' ]

                              Terminals [ '\002' ] ]
                          Terminals [ '\003' ] ])
                   (@"""1"" ""2"" ""3""",
                    Sequence
                        [ REString "1"
                          REString "2"
                          REString "3" ]) ]

          testCase "sequence group parsing tests"
          <| Helpers.parseAndCompare pSequenceGroup
                 [ (@"(%d1)", Terminals [ '\001' ])
                   (@"(%d1 %d2)",
                    Sequence
                        [ Terminals [ '\001' ]
                          Terminals [ '\002' ] ])
                   (@"(%d1 %d2 / %d3)",
                    Alternatives
                        [ Sequence
                            [ Terminals [ '\001' ]
                              Terminals [ '\002' ] ]
                          Terminals [ '\003' ] ])
                   (@"(""1"" ""2"" ""3"")",
                    Sequence
                        [ REString "1"
                          REString "2"
                          REString "3" ]) ]

          testCase "alternate parsing tests"
          <| Helpers.parseAndCompare pAlternates
                 [ (@"%d1", Terminals [ '\001' ])
                   (@"%d1 / %d2",
                    Alternatives
                        [ Terminals [ '\001' ]
                          Terminals [ '\002' ] ])
                   (@"%d1 / %d2 / %d3",
                    Alternatives
                        [ Terminals [ '\001' ]
                          Terminals [ '\002' ]
                          Terminals [ '\003' ] ])
                   (@"""1"" / ""2"" / ""3""",
                    Alternatives
                        [ REString "1"
                          REString "2"
                          REString "3" ]) ]

          testCase "optional group parsing tests"
          <| Helpers.parseAndCompare pOptionalGroup
                 [ (@"[%d1]",
                    OptionalSequence(Terminals [ '\001' ]))
                   (@"[%d1 %d2]",
                    OptionalSequence
                        (Sequence
                            [ Terminals [ '\001' ]
                              Terminals [ '\002' ] ]))
                   (@"[%d1 %d2 %d3]",
                    OptionalSequence
                        (Sequence
                            [ Terminals [ '\001' ]
                              Terminals [ '\002' ]
                              Terminals [ '\003' ] ]))
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
                        [ Terminals [ ' ' ]
                          Alternatives
                              [ REString "1"
                                REString "2" ]
                          Terminals [ ' ' ] ])
                   (@"[%d20 / (""1"" / ""2"")] / %b0101",
                    Alternatives
                        [ OptionalSequence
                            (Alternatives
                                [ Terminals [ '\020' ]
                                  Alternatives
                                      [ REString "1"
                                        REString "2" ] ])
                          Terminals [ '\005' ] ])
                   (@"%b0101 / [""1"" / ""2""] / %x20",
                    Alternatives
                        [ Terminals [ '\005' ]

                          OptionalSequence
                              (Alternatives
                                  [ REString "1"
                                    REString "2" ])
                          Terminals [ ' ' ] ])
                   (@"[%b0101 / [""1"" / ""2""] / %x20]",
                    OptionalSequence
                        (Alternatives
                            [ Terminals [ '\005' ]

                              OptionalSequence
                                  (Alternatives
                                      [ REString "1"
                                        REString "2" ])
                              Terminals [ ' ' ] ]))
                   (@"[%b0101 / (%x0F) / %x20]",
                    OptionalSequence
                        (Alternatives
                            [ Terminals [ '\005' ]
                              Terminals [ '\015' ]
                              Terminals [ ' ' ] ])) ]

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
                         [ Terminals
                             [ '\001'
                               '\002'
                               '\003'
                               '\004'
                               '\005'
                               '\006'
                               '\007'
                               '\b'
                               '\009' ]
                           Terminals [ '\011'; '\012' ]

                           Terminals
                               [ '\014'
                                 '\015'
                                 '\016'
                                 '\017'
                                 '\018'
                                 '\019'
                                 '\020'
                                 '\021'
                                 '\022'
                                 '\023'
                                 '\024'
                                 '\025'
                                 '\026'
                                 '\027'
                                 '\028'
                                 '\029'
                                 '\030'
                                 '\031' ]

                           Terminals
                               [ '!'
                                 '"'
                                 '#'
                                 '$'
                                 '%'
                                 '&'
                                 '\''
                                 '('
                                 ')'
                                 '*'
                                 '+'
                                 ','
                                 '-'
                                 '.'
                                 '/'
                                 '0'
                                 '1'
                                 '2'
                                 '3'
                                 '4'
                                 '5'
                                 '6'
                                 '7'
                                 '8'
                                 '9' ]
                           Terminals
                               [ for c in [ 0x3B .. 0xFF ] do
                                   yield char c ] ] ]))

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
                    ("SPACE", [ Terminals [ ' ' ] ]))
                   (@"crlf       =  %x0D %x0A   ; ""carriage return"" ""linefeed""",
                    ("crlf",
                     [ Sequence
                         [ Terminals [ '\013' ]
                           Terminals [ '\010' ] ] ]))
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
                         [ Terminals
                             [ '\001'
                               '\002'
                               '\003'
                               '\004'
                               '\005'
                               '\006'
                               '\007' ]
                           Terminals [ '\b'; '\009' ]
                           Terminals [ '\011'; '\012' ]

                           Terminals
                               [ '\014'
                                 '\015'
                                 '\016'
                                 '\017'
                                 '\018'
                                 '\019'
                                 '\020'
                                 '\021'
                                 '\022'
                                 '\023'
                                 '\024'
                                 '\025'
                                 '\026'
                                 '\027'
                                 '\028'
                                 '\029'
                                 '\030'
                                 '\031' ]

                           Terminals
                               [ '!'
                                 '"'
                                 '#'
                                 '$'
                                 '%'
                                 '&'
                                 '\''
                                 '('
                                 ')'
                                 '*'
                                 '+' ] ] ]))
                   (@"chanstring = %x2D-39 / %x3B-FF",
                    ("chanstring",
                     [ Alternatives
                         [ Terminals
                             [ '-'
                               '.'
                               '/'
                               '0'
                               '1'
                               '2'
                               '3'
                               '4'
                               '5'
                               '6'
                               '7'
                               '8'
                               '9' ]
                           Terminals
                               [ for c in [ 0x3B .. 0xFF ] do
                                   yield char c ] ] ]))

                   (@"channelid  = 5( %x41-5A / digit )   ",
                    ("channelid",
                     [ Repetition
                         (Exactly 5uy,
                          Alternatives
                              [ Terminals
                                  [ 'A'
                                    'B'
                                    'C'
                                    'D'
                                    'E'
                                    'F'
                                    'G'
                                    'H'
                                    'I'
                                    'J'
                                    'K'
                                    'L'
                                    'M'
                                    'N'
                                    'O'
                                    'P'
                                    'Q'
                                    'R'
                                    'S'
                                    'T'
                                    'U'
                                    'V'
                                    'W'
                                    'X'
                                    'Y'
                                    'Z' ]
                                RuleReference "digit" ]) ]))

                   (@"user       =  1*( %x01-09 / %x0B-0C / %x0E-1F / %x21-3F / %x41-FF )",
                    ("user",
                     [ Repetition
                         (AtLeast 1uy,
                          Alternatives
                              [ Terminals
                                  [ '\001'
                                    '\002'
                                    '\003'
                                    '\004'
                                    '\005'
                                    '\006'
                                    '\007'
                                    '\b'
                                    '\009' ]
                                Terminals [ '\011'; '\012' ]

                                Terminals
                                    [ '\014'
                                      '\015'
                                      '\016'
                                      '\017'
                                      '\018'
                                      '\019'
                                      '\020'
                                      '\021'
                                      '\022'
                                      '\023'
                                      '\024'
                                      '\025'
                                      '\026'
                                      '\027'
                                      '\028'
                                      '\029'
                                      '\030'
                                      '\031' ]

                                Terminals
                                    [ '!'
                                      '"'
                                      '#'
                                      '$'
                                      '%'
                                      '&'
                                      '\''
                                      '('
                                      ')'
                                      '*'
                                      '+'
                                      ','
                                      '-'
                                      '.'
                                      '/'
                                      '0'
                                      '1'
                                      '2'
                                      '3'
                                      '4'
                                      '5'
                                      '6'
                                      '7'
                                      '8'
                                      '9'
                                      ':'
                                      ';'
                                      '<'
                                      '='
                                      '>'
                                      '?' ]
                                Terminals
                                    [ for c in [ 0x41 .. 0xFF ] do
                                        yield char c ] ]) ]))

                   (@"key        =  1*23( %x01-05 / %x07-08 / %x0C / %x0E-1F / %x21-7F )",
                    ("key",
                     [ Repetition
                         (Between(1uy, 23uy),
                          Alternatives
                              [ Terminals
                                  [ '\001'
                                    '\002'
                                    '\003'
                                    '\004'
                                    '\005' ]
                                Terminals [ '\007'; '\b' ]
                                Terminals [ '\012' ]

                                Terminals
                                    [ '\014'
                                      '\015'
                                      '\016'
                                      '\017'
                                      '\018'
                                      '\019'
                                      '\020'
                                      '\021'
                                      '\022'
                                      '\023'
                                      '\024'
                                      '\025'
                                      '\026'
                                      '\027'
                                      '\028'
                                      '\029'
                                      '\030'
                                      '\031' ]

                                Terminals
                                    [ '!'
                                      '"'
                                      '#'
                                      '$'
                                      '%'
                                      '&'
                                      '\''
                                      '('
                                      ')'
                                      '*'
                                      '+'
                                      ','
                                      '-'
                                      '.'
                                      '/'
                                      '0'
                                      '1'
                                      '2'
                                      '3'
                                      '4'
                                      '5'
                                      '6'
                                      '7'
                                      '8'
                                      '9'
                                      ':'
                                      ';'
                                      '<'
                                      '='
                                      '>'
                                      '?'
                                      '@'
                                      'A'
                                      'B'
                                      'C'
                                      'D'
                                      'E'
                                      'F'
                                      'G'
                                      'H'
                                      'I'
                                      'J'
                                      'K'
                                      'L'
                                      'M'
                                      'N'
                                      'O'
                                      'P'
                                      'Q'
                                      'R'
                                      'S'
                                      'T'
                                      'U'
                                      'V'
                                      'W'
                                      'X'
                                      'Y'
                                      'Z'
                                      '['
                                      '\\'
                                      ']'
                                      '^'
                                      '_'
                                      '`'
                                      'a'
                                      'b'
                                      'c'
                                      'd'
                                      'e'
                                      'f'
                                      'g'
                                      'h'
                                      'i'
                                      'j'
                                      'k'
                                      'l'
                                      'm'
                                      'n'
                                      'o'
                                      'p'
                                      'q'
                                      'r'
                                      's'
                                      't'
                                      'u'
                                      'v'
                                      'w'
                                      'x'
                                      'y'
                                      'z'
                                      '{'
                                      '|'
                                      '}'
                                      '~'
                                      '\127' ] ]) ]))

                   (@"letter     =  %x41-5A / %x61-7A",
                    ("letter",
                     [ Alternatives
                         [ Terminals
                             [ 'A'
                               'B'
                               'C'
                               'D'
                               'E'
                               'F'
                               'G'
                               'H'
                               'I'
                               'J'
                               'K'
                               'L'
                               'M'
                               'N'
                               'O'
                               'P'
                               'Q'
                               'R'
                               'S'
                               'T'
                               'U'
                               'V'
                               'W'
                               'X'
                               'Y'
                               'Z' ]

                           Terminals
                               [ 'a'
                                 'b'
                                 'c'
                                 'd'
                                 'e'
                                 'f'
                                 'g'
                                 'h'
                                 'i'
                                 'j'
                                 'k'
                                 'l'
                                 'm'
                                 'n'
                                 'o'
                                 'p'
                                 'q'
                                 'r'
                                 's'
                                 't'
                                 'u'
                                 'v'
                                 'w'
                                 'x'
                                 'y'
                                 'z' ] ] ]))

                   (@"digit      =  %x30-39          ",
                    ("digit",
                     [ Terminals
                         [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ] ]))
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
                         [ Terminals
                             [ '['; '\\'; ']'; '^'; '_'; '`' ]
                           Terminals [ '{'; '|'; '}' ] ] ]))
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
                    ("wildone", [ Terminals [ '?' ] ]))
                   (@"wildmany   =  %x2A",
                    ("wildmany", [ Terminals [ '*' ] ]))
                   (@"nowild     =  %x01-29 / %x2B-3E / %x40-FF  ; any octet except NUL, ""*"", ""?""",
                    ("nowild",
                     [ Alternatives
                         [ Terminals
                             [ '\001'
                               '\002'
                               '\003'
                               '\004'
                               '\005'
                               '\006'
                               '\007'
                               '\b'
                               '\009'
                               '\010'
                               '\011'
                               '\012'
                               '\013'
                               '\014'
                               '\015'
                               '\016'
                               '\017'
                               '\018'
                               '\019'
                               '\020'
                               '\021'
                               '\022'
                               '\023'
                               '\024'
                               '\025'
                               '\026'
                               '\027'
                               '\028'
                               '\029'
                               '\030'
                               '\031'
                               ' '
                               '!'
                               '"'
                               '#'
                               '$'
                               '%'
                               '&'
                               '\''
                               '('
                               ')' ]

                           Terminals
                               [ '+'
                                 ','
                                 '-'
                                 '.'
                                 '/'
                                 '0'
                                 '1'
                                 '2'
                                 '3'
                                 '4'
                                 '5'
                                 '6'
                                 '7'
                                 '8'
                                 '9'
                                 ':'
                                 ';'
                                 '<'
                                 '='
                                 '>' ]
                           Terminals
                               [ for c in [ 0x40 .. 0xFF ] do
                                   yield char c ] ] ]))
                   (@"noesc      =  %x01-5B / %x5D-FF  ; any octet except NUL and ""\""",
                    ("noesc",
                     [ Alternatives
                         [ Terminals
                             [ '\001'
                               '\002'
                               '\003'
                               '\004'
                               '\005'
                               '\006'
                               '\007'
                               '\b'
                               '\009'
                               '\010'
                               '\011'
                               '\012'
                               '\013'
                               '\014'
                               '\015'
                               '\016'
                               '\017'
                               '\018'
                               '\019'
                               '\020'
                               '\021'
                               '\022'
                               '\023'
                               '\024'
                               '\025'
                               '\026'
                               '\027'
                               '\028'
                               '\029'
                               '\030'
                               '\031'
                               ' '
                               '!'
                               '"'
                               '#'
                               '$'
                               '%'
                               '&'
                               '\''
                               '('
                               ')'
                               '*'
                               '+'
                               ','
                               '-'
                               '.'
                               '/'
                               '0'
                               '1'
                               '2'
                               '3'
                               '4'
                               '5'
                               '6'
                               '7'
                               '8'
                               '9'
                               ':'
                               ';'
                               '<'
                               '='
                               '>'
                               '?'
                               '@'
                               'A'
                               'B'
                               'C'
                               'D'
                               'E'
                               'F'
                               'G'
                               'H'
                               'I'
                               'J'
                               'K'
                               'L'
                               'M'
                               'N'
                               'O'
                               'P'
                               'Q'
                               'R'
                               'S'
                               'T'
                               'U'
                               'V'
                               'W'
                               'X'
                               'Y'
                               'Z'
                               '[' ]
                           Terminals
                               [ for c in [ 0x5D .. 0xFF ] do
                                   yield char c ] ] ]))
                   (@"matchone   =  %x01-FF    ; matches wildone",
                    ("matchone",
                     [ Terminals
                         [ for c in [ 0x01 .. 0xFF ] do
                             yield char c ] ]))

                   (@"matchmany  =  *matchone    ; matches wildmany",
                    ("matchmany",
                     [ Repetition
                         (Any, RuleReference "matchone") ])) ] ]