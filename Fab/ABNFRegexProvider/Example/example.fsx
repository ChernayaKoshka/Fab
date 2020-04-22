#r @"..\bin\Debug\netstandard2.0\Fab.ABNFProvider.dll"
open Fab.ABNFRegexProvider

type PostalAddress = ABNFRegex< @"Fab\ABNFRegexProvider\Example\test.abnf" >

let address =
    "Test lastnamme Sr.\r\n123 Main St\r\nFakeTown, AA 12345\r\n"

let postalAdressCap = PostalAddress.``postal-address``.Match(address)

printfn "%b|%s" postalAdressCap.Success postalAdressCap.Groups.["street"].Value