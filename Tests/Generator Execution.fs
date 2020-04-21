module Generator_Execution_Tests

open FSharp.Data
open Expecto
open Fab

type ZIPCodeList = CsvProvider< @"Samples\free-zipcode-database-Primary.csv", Schema="Zipcode=string" >

let zipcodeRows =
    ZIPCodeList.GetSample().Rows

let zipcodes =
    zipcodeRows
    |> Seq.take 200
    |> Seq.map (fun row -> row.Zipcode)
    |> List.ofSeq

let zipparts =
    zipcodeRows
    |> Seq.take 200
    |> Seq.filter (fun row ->
        row.ZipCodeType.ToUpper() <> "MILITARY" &&
        not <| row.City.Contains("-") &&
        not <| row.City.Contains("/"))
    |> Seq.map (fun row ->
        sprintf "%s, %s %s\r\n" row.City row.State row.Zipcode)
    |> List.ofSeq

(*
    Retrieved From https://catalog.data.gov/dataset/addresses
    Resource Type Dataset
    Metadata Created Date February 10, 2017
    Metadata Updated Date April 5, 2019
    Publisher City of Chesapeake, VA
    Unique Identifier http://public-chesva.opendata.arcgis.com/datasets/b469c77f314242a0a602075e936d2ea7_18
    Maintainer Virginia Fowler
    Maintainer Email gisteam@cityofchesapeake.net
    License https://hub.arcgis.com/api/v2/datasets/b469c77f314242a0a602075e936d2ea7_18/license
*)
type AddressList = CsvProvider< @"Samples\Addresses.csv", InferRows = 0, IgnoreErrors = true >

let addressRows = AddressList.GetSample().Rows

let streets =
    addressRows
    |> Seq.take 200
    |> Seq.map (fun row -> row.ADDRESS + "\r\n")
    |> Seq.distinct
    |> List.ofSeq

[<Tests>]
let simpleStringParsing =
    testList "string parsing tests" [
        testCase "simple string parsing" (fun _ ->
            let parser =
                Helpers.run pRuleRecord "test = \"AbCdEfGhIjklmnopqrstuvwxYZ\""
                |> Helpers.generateSingle
            Helpers.expectWellFormedRegex parser

            [
                "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                "ABCDEFGHIJKLMNOPQRStuvwxyz"
                "abcdefghijklmnopqrsTUVWXYZ"
            ]
            |> List.iter (Helpers.expectSuccessfulMatch parser)
        )

        testCase "string with special characters properly parses" (fun _ ->
            let parser =
                Helpers.run pRuleRecord "test = \"\\\""
                |> Helpers.generateSingle
            Helpers.expectWellFormedRegex parser
            Helpers.expectSuccessfulMatch parser "\\"
        )
    ]

[<Tests>]
let simpleRuleParsing =
    testList "ruleset processing" [
        testCase "single rule parsing tests (zipcode)" (fun _ ->
            let parser =
                Helpers.run pRuleRecord "zip-code         = 5DIGIT [\"-\" 4DIGIT]"
                |> Helpers.generateSingle
            Helpers.expectWellFormedRegex parser
            zipcodes
            |> List.iter (Helpers.expectSuccessfulMatch parser)
        )
        testList "ruleset execution tests" [
            testCase "Simple ruleset parsing test street" <|
                Helpers.testRuleset
                    """
                    street           = [apt SP] house-num SP street-name CRLF
                    apt              = 1*4DIGIT
                    house-num        = 1*8(DIGIT / ALPHA)
                    street-name      = 1*(VCHAR / SP)
                    """
                    "street"
                    ("123 Main St\r\n" :: streets)
            testCase "Simple ruleset parsing test zip-part" <|
                Helpers.testRuleset
                    """
                    zip-part         = town-name "," SP state 1*2SP zip-code CRLF
                    town-name        = 1*(ALPHA / SP)
                    state            = 2ALPHA
                    zip-code         = 5DIGIT ["-" 4DIGIT]
                    """
                    "zip-part"
                    ("Test Town, AL 99210\r\n" :: zipparts)
        ]
    ]

[<Tests>]
let backtracking =
    testCase "Simple backtracking" (fun _ ->
        let parser =
                Helpers.run pRuleRecord "test = *(ALPHA) ALPHA"
                |> Helpers.generateSingle
        Helpers.expectWellFormedRegex parser
        [ "ABC" ]
        |> List.iter (Helpers.expectSuccessfulMatch parser)
    )

[<Tests>]
let complexRuleProcessing =
    testList "Complex ruleset parsing test" [
        testCase "full postal address"<|
            Helpers.testRuleset
                """
                postal-address   = name-part street zip-part

                name-part        = *(personal-part SP) last-name [SP suffix] CRLF
                name-part        =/ personal-part CRLF

                personal-part    = first-name / (initial ".")
                first-name       = *ALPHA
                initial          = ALPHA
                last-name        = *ALPHA
                suffix           = ("Jr." / "Sr." / 1*("I" / "V" / "X"))

                street           = [apt SP] house-num SP street-name CRLF
                apt              = 1*4DIGIT
                house-num        = 1*8(DIGIT / ALPHA)
                street-name      = 1*VCHAR

                zip-part         = town-name "," SP state 1*2SP zip-code CRLF
                town-name        = 1*(ALPHA / SP)
                state            = 2ALPHA
                zip-code         = 5DIGIT ["-" 4DIGIT]
                """
                "postal-address"
                [ "Test lastnamme Sr.\r\n123 Main St\r\nFakeTown, AA 12345\r\n" ]
    ]