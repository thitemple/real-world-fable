module App

open Fable.React

type Model = string

type Msg = Msg of unit

let init() = "Hello World from Fable and F#"

let update msg model = model

let rootView model dispatch = div [] [ str model ]

open Elmish
open Elmish.HMR

Program.mkSimple init update rootView
|> Program.withReactSynchronous "real-world-fable-app"
|> Program.withConsoleTrace
|> Program.run
