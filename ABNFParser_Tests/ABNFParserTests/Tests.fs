module Tests

open Expecto
open ABNF.Types
open ABNF.CoreRules
open ABNF.Grammar

[<Tests>]
let tests =
  testList "simple parsing tests" [
    testCase "universe exists" <| fun _ ->
      let subject = true
      Expect.isTrue subject "I compute, therefore I am."

    testCase "should fail" <| fun _ ->
      let subject = false
      Expect.isTrue subject "I should fail because the subject is false."
  ]