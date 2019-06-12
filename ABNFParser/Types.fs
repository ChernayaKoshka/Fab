module Types

type RuleName = string

type Terminal = char

type Range =
    {
        Start   : uint8 option
        IsRange : bool
        End     : uint8 option
    }

/// Strings, names formation
/// Comment
/// Value range
/// Repetition
/// Grouping, optional
/// Concatenation
/// Alternative
type Rule =
    {
        RuleName : RuleName
        Definition : Element
    }
and Element =
    | Comment          of string          // ; comment
    | Rule             of Rule            // rulename = definition
    | Terminals        of Terminal list   // %x20, %x20.21, %x20-21
    | Reference        of RuleName        // rulename = reference-name
    | Alternatives     of Element list    // %x20 / %x21
    | OptionalSequence of Element         // [optional]
    | Group            of Element list    // (%20 %21)
    | Sequence         of Element list    // a b c
    | Repetition       of Range * Element // 2*3(%20 %21)
    | RuleReference    of string