module Pages.NewPost

open Elmish
open Fable.React
open Fable.React.Props


// TYPES

type Model = string

type Msg = Msg

// COMMANDS


// STATE

let init() = "", Cmd.none

let update (msg: Msg) (model: Model) = model, Cmd.none


// VIEW

let view dispatch (model: Model) = div [] [ str "New Post" ]
 