[<AutoOpen>]
module Fab.Types

open FParsec

type RuleName = string

type Range =
    | Any
    | AtLeast of uint8
    | AtMost of uint8
    | Exactly of uint8
    | Between of (uint8 * uint8)

type CoreRule =
    | ALPHA
    | DIGIT
    | HEXDIG
    | DQUOTE
    | SP
    | HTAB
    | WSP
    | CR
    | LF
    | CRLF
    | LWSP
    | VCHAR
    | CHAR
    | OCTET
    | CTL
    | BIT

type Terminal = char

type TerminalGrouping =
    | TerminalSingle of Terminal
    | TerminalRange of Terminal * Terminal
    | TerminalGroup of Terminal list

type RuleElement =
    | REString           of string
    | Terminals        of TerminalGrouping        // %x20, %x20.21, %x20-21
    | CoreRule         of CoreRule
    | Alternatives     of RuleElement list    // %x20 / %x21
    | OptionalSequence of RuleElement         // [optional]
    | Sequence         of RuleElement list    // a b c or (%20 %21)
    | Repetition       of Range * RuleElement // 2*3(%20 %21)
    | RuleReference    of string
    with
        override this.ToString() =
            match this with
            | REString str -> sprintf "String(%s)" str
            // | Terminals terms -> sprintf "Terminals [%d..%d]" (int terms.[terms.Length-1]) (int terms.[0])
            | CoreRule rule -> rule.ToString()
            | Alternatives elems -> sprintf "Alternatives: %s" ((sprintf "%O" elems).Replace("\r", "").Replace("\n", "  "))
            | OptionalSequence elems -> sprintf "OptionalSequence: %s" ((sprintf "%O" elems).Replace("\r", "").Replace("\n", "  "))
            | Sequence elems -> sprintf "Sequence: %s" ((sprintf "%O" elems).Replace("\r", "").Replace("\n", "  "))
            | Repetition (range, element) -> sprintf "Repetition [%A:%s]" range (element.ToString())
            | RuleReference refs -> sprintf "RuleReference [%s]" refs

type Rule =
    {
        RuleName : RuleName
        Definition : RuleElement list
    }

type DefinedRules = RuleName list

type Parser<'t> = Parser<'t, DefinedRules>

type AST =
    | Rule of Rule
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