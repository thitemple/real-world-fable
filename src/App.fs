module App

open Elmish
open Elmish.HMR

Program.mkProgram State.init State.update View.rootView
|> Program.withReactSynchronous "real-world-fable-app"
|> Program.withConsoleTrace
|> Program.run
