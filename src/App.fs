module App

open Elmish
open Elmish.HMR
open Elmish.Navigation
open Thoth.Json

open Router
open Types
open State
open RootView

let private getSessionFromLocalStorage _ =
    Browser.WebStorage.localStorage.getItem "session"
    |> Option.ofObj
    |> Option.map (Decode.fromString Session.Decoder)
    |> Option.map (fun res ->
        match res with
        | Ok session -> Cmd.OfFunc.result <| SessionLoaded session

        | _ -> Cmd.none)
    |> Option.defaultValue Cmd.none


Program.mkProgram init update rootView
|> Program.withSubscription getSessionFromLocalStorage
|> Program.toNavigable (UrlParser.parseHash pageParser) setRoute
|> Program.withReactSynchronous "real-world-fable-app"
|> Program.withConsoleTrace
|> Program.run
