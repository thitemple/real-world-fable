module Elements

open Fable.React
open Fable.React.Props

let errorsList errors =
    let errorItem e = li [] [ str e ]
    ul [ ClassName "error-messages" ] (List.map errorItem errors)
