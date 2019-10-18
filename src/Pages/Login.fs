module Pages.Login

open Elmish
open Fable.React
open Fable.React.Props
open Fable.RemoteData

open Router
open Types
open Api
open Elements

// TYPES

type Model =
    { Email: string
      Password: string
      Errors: string list }

type ExternalMsg =
    | NoOp
    | SignedIn of Session

type Msg =
    | SetEmail of string
    | SetPassword of string
    | Submit
    | HandleLoginResponse of RemoteData<string list, Session>


// COMMANDS

let private login model =
    Cmd.OfAsync.perform Users.login
        {| email = model.Email
           password = model.Password |} HandleLoginResponse

// STATE

let init(): Model * Cmd<Msg> =
    { Email = ""
      Password = ""
      Errors = [] }, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | SetEmail email -> { model with Email = email }, Cmd.none, NoOp

    | SetPassword password -> { model with Password = password }, Cmd.none, NoOp

    | Submit -> model, login model, NoOp

    | HandleLoginResponse res ->
        match res with
        | Failure err -> { model with Errors = err }, Cmd.none, NoOp

        | Success(session) -> model, Cmd.none, SignedIn session

        | _ -> model, Cmd.none, NoOp


// VIEW

let private form dispatch model =
    form [ OnSubmit(fun _ -> dispatch Submit) ]
        [ fieldset [ ClassName "form-group" ]
              [ input
                  [ ClassName "form-control form-control-lg"
                    Type "text"
                    Value model.Email
                    OnChange(fun ev -> dispatch <| SetEmail ev.Value)
                    Placeholder "Email" ] ]

          fieldset [ ClassName "form-group" ]
              [ input
                  [ ClassName "form-control form-control-lg"
                    Type "password"
                    Value model.Password
                    OnChange(fun ev -> dispatch <| SetPassword ev.Value)
                    Placeholder "Password" ] ]

          button [ ClassName "btn btn-lg btn-primary pull-xs-right" ] [ str "Sign in" ] ]

let view dispatch model =
    div [ ClassName "auth-page" ]
        [ div [ ClassName "container page" ]
              [ div [ ClassName "row" ]
                    [ div [ ClassName "col-md-6 offset-md-3 col-xs-12" ]
                          [ h1 [ ClassName "text-xs-center" ] [ str "Sign in" ]

                            p [ ClassName "text-xs-center" ] [ a [ href Register ] [ str "Need an account?" ] ]

                            errorsList model.Errors

                            form dispatch model ] ] ] ]
