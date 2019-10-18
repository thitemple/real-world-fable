module Pages.NewPost

open Elmish
open Fable.React
open Fable.React.Props

type Model = string

type Msg = Msg

let init () =
    "", Cmd.none

let update (msg : Msg) (model : Model) =
    model, Cmd.none

let view dispatch ( model : Model ) =
    div [] [ str "New Post" ] 