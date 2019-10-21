module App

open Elmish
open Elmish.HMR
open Elmish.Navigation
open Thoth.Json

open Router
open Types
open State
open RootView

let private tryGetSessionFromLocalStorage =
    Browser.WebStorage.localStorage.getItem "session"
    |> Option.ofObj
    |> Option.bind (Decode.fromString Session.Decoder >> Result.toOption)


Program.mkProgram (init tryGetSessionFromLocalStorage) update rootView
|> Program.toNavigable (UrlParser.parseHash pageParser) setRoute
|> Program.withReactSynchronous "real-world-fable-app"
|> Program.withConsoleTrace
|> Program.run
