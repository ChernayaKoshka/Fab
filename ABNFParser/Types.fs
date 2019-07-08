[<AutoOpen>]
module ABNF.Types

type RuleName = string

type Terminal = char

type Range =
    | Any
    | AtLeast of uint8
    | AtMost of uint8
    | Exactly of uint8
    | Between of (uint8 * uint8)

type RuleElement =
    | Terminals        of Terminal list   // %x20, %x20.21, %x20-21
    | Alternatives     of RuleElement list    // %x20 / %x21
    | OptionalSequence of RuleElement         // [optional]
    | Sequence         of RuleElement list    // a b c or (%20 %21)
    | Repetition       of Range * RuleElement // 2*3(%20 %21)
    | RuleReference    of string

type ABNFRule =
    {
        RuleName : RuleName
        Definition : RuleElement list
    }

type AST =
    | Rule of ABNFRule
    | Element of RuleElement

type Peek =
    | Next of char
    | EOF

type RuleStream =
    {
        Text : string
        Pos  : int
    }
    member this.Peek() =
        if this.Pos < this.Text.Length then
            Next this.Text.[this.Pos]
        else
            EOF
