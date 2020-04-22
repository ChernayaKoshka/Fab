module Fab.ABNFProviderImplementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open System.Text.RegularExpressions
open ProviderImplementation
open ProviderImplementation.ProvidedTypes

open Fab
open Fab.Generator

// Put any runtime constructs here
type DataSource(filename:string) =
    member this.FileName = filename

[<TypeProvider>]
type ABNFRegexProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, addDefaultProbingLocation=true)

    let ns = "Fab.ABNFRegexProvider"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<DataSource>.Assembly.GetName().Name = asm.GetName().Name)

    let createTypes () =
        let staticParams = [ProvidedStaticParameter("filename", typeof<string>)]
        let abnfType = ProvidedTypeDefinition(asm, ns, "ABNFRegex", Some typeof<obj>)
        abnfType.DefineStaticParameters(
            parameters = staticParams,
            instantiationFunction = (fun typeName parameterValues ->
                let provided = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, hideObjectMethods = true)
                match parameterValues with
                | [| :? string as fileName |] ->
                    fileName
                    |> File.ReadAllText
                    |> parseAllRules
                    |> function
                    | Ok rules -> rules
                    | Error err -> failwith err
                    |> generate
                    |> Map.toArray
                    |> Array.iter (fun (name, regex) ->
                        ProvidedProperty(name, typeof<Regex>, (fun _ -> <@@Regex(regex)@@>), isStatic = true)
                        |> provided.AddMember
                    )
                    provided
                | other -> failwithf "%A was unexpected!" other
            )
        )

        [abnfType]

    do
        this.AddNamespace(ns, createTypes())

[<TypeProviderAssembly>]
do ()