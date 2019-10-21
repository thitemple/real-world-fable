module Pages.Register

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
    { Username: string
      Email: string
      Password: string
      Errors: string list }

type ExternalMsg =
    | NoOp
    | UserCreated of Session

type Msg =
    | SetUsername of string
    | SetEmail of string
    | SetPassword of string
    | Submit
    | HandleCreateResponse of RemoteData<string list, Session>


// COMMANDS

let private createUser model =
    Cmd.OfAsync.perform Users.createUser
        {| username = model.Username
           email = model.Email
           password = model.Password |} HandleCreateResponse

// STATE

let init(): Model * Cmd<Msg> =
    { Username = ""
      Email = ""
      Password = ""
      Errors = [] }, Cmd.none


let update msg (model: Model) =
    match msg with
    | SetUsername username -> { model with Username = username }, Cmd.none, NoOp

    | SetEmail email -> { model with Email = email }, Cmd.none, NoOp

    | SetPassword password -> { model with Password = password }, Cmd.none, NoOp

    | Submit -> model, createUser model, NoOp

    | HandleCreateResponse data ->
        match data with
        | Failure err -> { model with Errors = err }, Cmd.none, NoOp

        | Success(session) -> model, Cmd.none, UserCreated session

        | _ -> model, Cmd.none, NoOp


// VIEW

let private form dispatch model =
    form [ OnSubmit(fun _ -> dispatch Submit) ]
        [ fieldset [ ClassName "form-group" ]
              [ input
                  [ ClassName "form-control form-control-lg"
                    Type "text"
                    Value model.Username
                    OnChange(fun ev -> dispatch <| SetUsername ev.Value)
                    Placeholder "Your Username" ] ]

          fieldset [ ClassName "form-group" ]
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

          button [ ClassName "btn btn-lg btn-primary pull-xs-right" ] [ str "Sign up" ] ]


let view dispatch model =
    div [ ClassName "auth-page" ]
        [ div [ ClassName "container page" ]
              [ div [ ClassName "row" ]
                    [ div [ ClassName "col-md-6 offset-md-3 col-xs-12" ]
                          [ h1 [ ClassName "text-xs-center" ] [ str "Sign in" ]

                            p [ ClassName "text-xs-center" ] [ a [ href Login ] [ str "Have an account?" ] ]

                            errorsList model.Errors

                            form dispatch model ] ] ] ]
