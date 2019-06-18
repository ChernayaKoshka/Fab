module Expecto.FParsec
    open FParsec
    [<RequireQualifiedAccess>]
    module Expect =
        let isSuccess x message =
            match x with
            | Success(_) -> ()
            | Failure(_) ->
                failwithf "%s. Expected Success _, was Failure(%A)." message x

        let isFailure x message =
            match x with
            | Success(_) ->
                failwithf "%s. Expected Failure _, was Success(%A)." message x
            | Failure(_) -> ()