module Pages.Register

open System
open Elmish
open Fable.React
open Fable.React.Props
open Fable.RemoteData

open Shared.Router
open Shared.Types
open Shared.Api

type Model =
    { Username: string
      Email: string
      Password: string
      Errors: (string * string list) list }

type ExternalMsg =
    | NoOp
    | UserCreated of Session

type Msg =
    | SetUsername of string
    | SetEmail of string
    | SetPassword of string
    | Submit
    | HandleCreateResponse of RemoteData<Map<string, string list>, User * Session>

let init(): Model * Cmd<Msg> =
    { Username = ""
      Email = ""
      Password = ""
      Errors = [] }, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | SetUsername username -> { model with Username = username }, Cmd.none, NoOp
    | SetEmail email -> { model with Email = email }, Cmd.none, NoOp
    | SetPassword password -> { model with Password = password }, Cmd.none, NoOp
    | Submit ->
        model,
        Cmd.OfAsync.perform Users.createUser
            {| username = model.Username
               email = model.Email
               password = model.Password |} HandleCreateResponse, NoOp
    | HandleCreateResponse data ->
        match data with
        | Failure err -> { model with Errors = Map.toList err }, Cmd.none, NoOp
        | Success(_, session) -> model, Cmd.none, UserCreated session
        | _ -> model, Cmd.none, NoOp

let view dispatch model =
    div [ ClassName "auth-page" ]
        [ div [ ClassName "container page" ]
              [ div [ ClassName "row" ]
                    [ div [ ClassName "col-md-6 offset-md-3 col-xs-12" ]
                          [ h1 [ ClassName "text-xs-center" ] [ str "Sign in" ]
                            p [ ClassName "text-xs-center" ] [ a [ href Login ] [ str "Have an account?" ] ]

                            ul [ ClassName "error-messages" ]
                                (List.map (fun (key, err) -> li [] [ str <| sprintf "%s %s" key (List.head err) ])
                                     model.Errors)

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
                                  button [ ClassName "btn btn-lg btn-primary pull-xs-right" ] [ str "Sign up" ] ] ] ] ] ]
