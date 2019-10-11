module App

open Elmish
open Elmish.HMR

Program.mkSimple State.init State.update View.rootView
|> Program.withReactSynchronous "real-world-fable-app"
|> Program.withConsoleTrace
|> Program.run
