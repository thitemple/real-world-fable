module App

open Elmish
open Elmish.HMR

Router.mkProgramWithNavigation Routes.Articles Routes.Articles Types.NavigateTo Routes.pageParser State.init State.update View.rootView
|> Program.withReactSynchronous "real-world-fable-app"
|> Program.withConsoleTrace
|> Program.run
