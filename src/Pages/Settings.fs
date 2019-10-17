module Pages.Settings

open Elmish
open Fable.React
open Fable.React.Props
open Fable.RemoteData
open Thoth.Json

open Shared.Types
open Shared.Api

type FormData =
    { Avatar: string
      Bio: string
      Email: string
      Username: string
      Password: string }

type Model =
    { Session: Session
      FormData: RemoteData<exn, FormData> }

type Msg =
    | FormLoaded of RemoteData<exn, FormData>
    | SetAvatar of string
    | SetBio of string
    | SetUsername of string
    | SetEmail of string
    | SetPassword of string

let FormDecoder: Decoder<FormData> =
    Decode.object <| fun get ->
        { Avatar = get.Optional.Field "image" Decode.string |> Option.defaultValue ""
          Bio = get.Optional.Field "bio" Decode.string |> Option.defaultValue ""
          Email = get.Required.Field "email" Decode.string
          Username = get.Required.Field "username" Decode.string
          Password = "" }

let fetchUser session = Cmd.OfAsync.perform (Users.fetchUserWithDecoder FormDecoder) session FormLoaded

let init session: Model * Cmd<Msg> =
    { Session = session
      FormData = Loading }, fetchUser session

let updateForm transform model =
    match model.FormData with
    | Success formData -> { model with FormData = Success <| transform formData }, Cmd.none
    | _ -> model, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | FormLoaded data -> { model with FormData = data }, Cmd.none
    | SetAvatar avatar -> updateForm (fun formData -> { formData with Avatar = avatar }) model
    | SetBio bio -> updateForm (fun formData -> { formData with Bio = bio }) model
    | SetUsername username -> updateForm (fun formData -> { formData with Username = username }) model
    | SetEmail email -> updateForm (fun formData -> { formData with Email = email }) model
    | SetPassword password -> updateForm (fun formData -> { formData with Password = password }) model

let form dispatch (formData: FormData) =
    form []
        [ fieldset [ ClassName "form-group" ]
              [ input
                  [ ClassName "form-control"
                    Type "text"
                    Value formData.Avatar
                    OnChange(fun ev -> dispatch <| SetAvatar ev.Value)
                    Placeholder "URL of profile picture" ] ]
          fieldset [ ClassName "form-group" ]
              [ input
                  [ ClassName "form-control form-control-lg"
                    Type "text"
                    Value formData.Username
                    OnChange(fun ev -> dispatch <| SetUsername ev.Value)
                    Placeholder "Your Name" ] ]
          fieldset [ ClassName "form-group" ]
              [ textarea
                  [ ClassName "form-control form-control-lg"
                    Rows 8
                    Value formData.Bio
                    OnChange(fun ev -> dispatch <| SetBio ev.Value)
                    Placeholder "Short bio about you" ] [] ]
          fieldset [ ClassName "form-group" ]
              [ input
                  [ ClassName "form-control form-control-lg"
                    Type "text"
                    Value formData.Email
                    OnChange(fun ev -> dispatch <| SetEmail ev.Value)
                    Placeholder "Email" ] ]
          fieldset [ ClassName "form-group" ]
              [ input
                  [ ClassName "form-control form-control-lg"
                    Type "password"
                    Value formData.Password
                    OnChange(fun ev -> dispatch <| SetPassword ev.Value)
                    Placeholder "Password" ] ]
          button [ ClassName "btn btn-lg btn-primary pull-xs-right" ] [ str "Update Settings" ] ]

let view dispatch (model: Model) =
    div [ ClassName "settings-page" ]
        [ div [ ClassName "container page" ]
              [ div [ ClassName "row" ]
                    [ div [ ClassName "col-md-6 offset-md-3 col-xs-12" ]
                          [ h1 [ ClassName "text-xs-center" ] [ str "Your Settings" ]
                            (match model.FormData with
                             | Success formData -> form dispatch formData
                             | _ -> str "") ] ] ] ]
