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
    | Group            of RuleElement list    // (%20 %21)
    | Sequence         of RuleElement list    // a b c
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

type RuleStream =
    {
        Text : string
        Pos  : int
    }
    member this.Peek() =
        this.Text.[this.Pos]
