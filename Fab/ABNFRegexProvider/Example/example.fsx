#r @"..\bin\Debug\netstandard2.0\Fab.ABNFProvider.dll"
open Fab.ABNFRegexProvider

type PostalAddress = ABNFRegex< @"Fab\ABNFRegexProvider\Example\postal-address.abnf" >

let address =
    "Test lastnamme Sr.\r\n123 Main St\r\nFakeTown, AA 12345\r\n"

let postalAdressCap = PostalAddress.``postal-address``.Match(address)

printfn "%b|%s" postalAdressCap.Success postalAdressCap.Groups.["street"].Value

type IRCMessage = ABNFRegex< @"Fab\ABNFRegexProvider\Example\irc.abnf" >

let testMessage = ":WiZ!jto@tolsun.oulu.fi NICK Kilroy\r\n"

IRCMessage.message.Match(testMessage)