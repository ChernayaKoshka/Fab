[<AutoOpen>]
module ABNF.CoreRules

open FParsec

type Parser<'t> = Parser<'t, unit>

///Rule      Formal definition                              Meaning
///ALPHA      %x41-5A / %x61-7A                             Upper- and lower-case ASCII letters (A–Z, a–z)
let ALPHA =
    [0x41..0x5A] @ [0x61..0x7A]
    |> List.map char

///Rule      Formal definition                              Meaning
///DIGIT      %x30-39                                       Decimal digits (0–9)
let DIGIT =
    ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

///Rule      Formal definition                              Meaning
///HEXDIG     DIGIT / "A" / "B" / "C" / "D" / "E" / "F"     Hexadecimal digits (0–9, A–F)
let HEXDIG =
    DIGIT @ [ 'A'; 'B'; 'C' ; 'D'; 'E'; 'F']

///Rule      Formal definition                              Meaning
///DQUOTE     %x22                                          Double quote
let DQUOTE = '"'

///Rule      Formal definition                              Meaning
///SP         %x20                                          Space
let SP = ' '

///Rule      Formal definition                              Meaning
///HTAB       %x09                                          Horizontal tab
let HTAB = '\t'

///Rule      Formal definition                              Meaning
///WSP        SP / HTAB                                     Space and horizontal tab
let WSP = [SP; HTAB]

///Rule      Formal definition                              Meaning
///CR         %x0D                                          Carriage return
let CR = '\r'
///Rule      Formal definition                              Meaning
///LF         %x0A                                          Linefeed
let LF = '\n'
///Rule      Formal definition                              Meaning
///CRLF       CR LF                                         Internet-standard newline
let CRLF = "\r\n"

///Rule      Formal definition                              Meaning
///LWSP       *(WSP / CRLF WSP)                             Linear white space (past newline)
let LWSP : Parser<_>  =
    many ((anyOf WSP) <|> newline)

///Rule      Formal definition                              Meaning
///VCHAR      %x21-7E                                       Visible (printing) characters
let VCHAR =
    [0x21..0x7E]
    |> List.map char

///Rule      Formal definition                              Meaning
///CHAR       %x01-7F                                       Any ASCII character, excluding NUL
let CHAR =
    [0x01..0x7F]
    |> List.map char

///Rule      Formal definition                              Meaning
///OCTET      %x00-FF                                       8 bits of data
let OCTET =
    [0x00..0xFF]
    |> List.map char

///Rule      Formal definition                              Meaning
///CTL        %x00-1F / %x7F                                Controls
let CTL =
    [0x00..0x1F] @ [0x7F]
    |> List.map char

///Rule      Formal definition                              Meaning
///BIT        "0" / "1"                                     Binary digit
let BIT =
    ['0'; '1']