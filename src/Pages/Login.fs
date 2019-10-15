module Pages.Login

open Elmish
open Fable.React
open Fable.React.Props

open Shared.Router

type Model = string

type Msg = Msg

let init() = "", Cmd.none

let update msg model: Model * Cmd<Msg> = model, Cmd.none

let view dispatch model =
    div [ ClassName "auth-page" ]
        [ div [ ClassName "container page" ]
              [ div [ ClassName "row" ]
                    [ div [ ClassName "col-md-6 offset-md-3 col-xs-12" ]
                          [ h1 [ ClassName "text-xs-center" ] [ str "Sign in" ]
                            p [ ClassName "text-xs-center" ] [ a [ href Register ] [ str "Need an account?" ] ]

                            ul [ ClassName "error-messages" ] [ li [] [ str "Some error" ] ]

                            form []
                                [ fieldset [ ClassName "form-group" ]
                                      [ input
                                          [ ClassName "form-control form-control-lg"
                                            Type "text"
                                            Placeholder "Email" ] ]
                                  fieldset [ ClassName "form-group" ]
                                      [ input
                                          [ ClassName "form-control form-control-lg"
                                            Type "password"
                                            Placeholder "Password" ] ]
                                  button [ ClassName "btn btn-lg btn-primary pull-xs-right" ] [ str "Sign in" ] ] ] ] ] ]
