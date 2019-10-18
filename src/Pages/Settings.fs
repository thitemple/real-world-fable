module Pages.Settings

open System
open Elmish
open Fable.React
open Fable.React.Props
open Fable.RemoteData

open Types
open Router
open Types.User
open Api
open Elements


// TYPES

type Model =
    { Session: Session
      Password: string
      User: RemoteData<string list, User>
      Errors: string list }

type Msg =
    | UserFetched of RemoteData<string list, User>
    | UserSaved of RemoteData<string list, User>
    | SetImage of string
    | SetBio of string
    | SetUsername of string
    | SetEmail of string
    | SetPassword of string
    | Submit


// COMMANDS

let private fetchUser session = Cmd.OfAsync.perform Users.fetchUser session UserFetched

let private updateUser session validatedUser password =
    Cmd.OfAsync.perform (Users.updateUser session) (validatedUser, password) UserSaved

// STATE

let init session =
    { Session = session
      Password = ""
      User = Loading
      Errors = [] }, fetchUser session


let private updateForm transform model =
    match model.User with
    | Success formData -> { model with User = Success <| transform formData }, Cmd.none

    | _ -> model, Cmd.none

let update msg model =
    match msg with
    | UserFetched data -> { model with User = data }, Cmd.none

    | SetImage image ->
        updateForm (fun formData ->
            { formData with
                  Image =
                      if String.IsNullOrWhiteSpace image then None
                      else Some image }) model

    | SetBio bio ->
        updateForm (fun formData ->
            { formData with
                  Bio =
                      if String.IsNullOrWhiteSpace bio then None
                      else Some bio }) model

    | SetUsername username -> updateForm (fun formData -> { formData with Username = username }) model

    | SetEmail email -> updateForm (fun formData -> { formData with Email = email }) model

    | SetPassword password -> { model with Password = password }, Cmd.none

    | Submit ->
        match model.User with
        | Success user ->
            let result = validateUser user

            match result with
            | Ok validatedUser -> model, updateUser model.Session validatedUser model.Password

            | Error err -> { model with Errors = [ err ] }, Cmd.none

        | _ -> model, Cmd.none

    | UserSaved(Success _) -> model, newUrl Articles

    | UserSaved(Failure e) -> { model with Errors = e }, Cmd.none

    | UserSaved _ -> model, Cmd.none


// VIEW

let private form dispatch (user: User) password =
    form [ OnSubmit(fun _ -> dispatch Submit) ]
        [ fieldset [ ClassName "form-group" ]
              [ input
                  [ ClassName "form-control"
                    Type "text"
                    Value user.Image
                    OnChange(fun ev -> dispatch <| SetImage ev.Value)
                    Placeholder "URL of profile picture" ] ]

          fieldset [ ClassName "form-group" ]
              [ input
                  [ ClassName "form-control form-control-lg"
                    Type "text"
                    Value user.Username
                    OnChange(fun ev -> dispatch <| SetUsername ev.Value)
                    Placeholder "Your Name" ] ]

          fieldset [ ClassName "form-group" ]
              [ textarea
                  [ ClassName "form-control form-control-lg"
                    Rows 8
                    Value user.Bio
                    OnChange(fun ev -> dispatch <| SetBio ev.Value)
                    Placeholder "Short bio about you" ] [] ]

          fieldset [ ClassName "form-group" ]
              [ input
                  [ ClassName "form-control form-control-lg"
                    Type "text"
                    Value user.Email
                    OnChange(fun ev -> dispatch <| SetEmail ev.Value)
                    Placeholder "Email" ] ]

          fieldset [ ClassName "form-group" ]
              [ input
                  [ ClassName "form-control form-control-lg"
                    Type "password"
                    Value password
                    OnChange(fun ev -> dispatch <| SetPassword ev.Value)
                    Placeholder "Password" ] ]

          button [ ClassName "btn btn-lg btn-primary pull-xs-right" ] [ str "Update Settings" ] ]

let private renderForm dispatch model =
    match model.User with
    | Success formData -> form dispatch formData model.Password

    | _ -> empty

let view dispatch (model: Model) =
    div [ ClassName "settings-page" ]
        [ div [ ClassName "container page" ]
              [ div [ ClassName "row" ]
                    [ div [ ClassName "col-md-6 offset-md-3 col-xs-12" ]
                          [ h1 [ ClassName "text-xs-center" ] [ str "Your Settings" ]

                            errorsList model.Errors

                            renderForm dispatch model ] ] ] ]
