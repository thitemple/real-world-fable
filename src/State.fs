module State

open Elmish
open Thoth.Json

open Router
open Types


// TYPES

type Page =
    | NotFound
    | Loading
    | Login of Pages.Login.Model
    | Register of Pages.Register.Model
    | Articles of Pages.Articles.Model
    | Article of Pages.Article.Model
    | Settings of Pages.Settings.Model
    | NewPost of Pages.NewPost.Model

type Msg =
    | ArticlesMsg of Pages.Articles.Msg
    | ArticleMsg of Pages.Article.Msg
    | LoginMsg of Pages.Login.Msg
    | RegisterMsg of Pages.Register.Msg
    | SettingsMsg of Pages.Settings.Msg
    | NewPostMsg of Pages.NewPost.Msg
    | NoOp

type Model =
    { CurrentRoute: Route option
      ActivePage: Page
      Session: Types.Session option }


// STATE

let setSessionRoute sessionRoute model =
    match model.Session with
    | None -> model, newUrl Route.Login

    | Some session ->
        match sessionRoute with
        | Logout ->
            { model with Session = None },
            Cmd.batch
                [ newUrl <| Route.Article ArticlesList
                  Cmd.OfFunc.perform (fun _ -> Browser.WebStorage.localStorage.removeItem ("session")) ()
                      (fun _ -> NoOp) ]

        | SessionRoute.Settings ->
            let settingsModel, settingsCmd = Pages.Settings.init session
            { model with ActivePage = Settings settingsModel }, Cmd.map SettingsMsg settingsCmd

        | SessionRoute.NewPost ->
            let newPostModel, newPostCmd = Pages.NewPost.init()
            { model with ActivePage = NewPost newPostModel }, Cmd.map NewPostMsg newPostCmd

let setRoute result model =
    let model = { model with CurrentRoute = result }

    match result with
    | None -> { model with ActivePage = NotFound }, Cmd.none

    | Some route ->
        match route with
        | SessionRoute sessionRoute -> setSessionRoute sessionRoute model

        | Route.Article(ArticlesList) ->
            let articlesModel, articlesCmd = Pages.Articles.init()
            { model with ActivePage = Articles articlesModel }, Cmd.map ArticlesMsg articlesCmd

        | Route.Article(ArticleRoute.Article slug) ->
            let articleModel, articleCmd = Pages.Article.init slug
            { model with ActivePage = Article articleModel }, Cmd.map ArticleMsg articleCmd

        | Route.Login ->
            let loginModel, loginCmd = Pages.Login.init()
            { model with ActivePage = Login loginModel }, Cmd.map LoginMsg loginCmd

        | Route.Register ->
            let registerModel, registerCmd = Pages.Register.init()
            { model with ActivePage = Register registerModel }, Cmd.map RegisterMsg registerCmd

let init session (route: Route option): Model * Cmd<Msg> =
    { CurrentRoute = None
      ActivePage = Loading
      Session = session }
    |> setRoute route

let private saveSession (session: Session) =
    Cmd.OfFunc.perform (fun s ->
        let sessionStr = Encode.Auto.toString (0, s, isCamelCase = true)
        Browser.WebStorage.localStorage.setItem ("session", sessionStr)) session (fun _ -> NoOp)

let update msg model: Model * Cmd<Msg> =

    match msg with
    | ArticlesMsg articlesMsg ->
        match model.ActivePage with
        | Articles articlesModel ->
            let articlesModel, articlesCmd = Pages.Articles.update articlesMsg articlesModel
            { model with ActivePage = (Articles articlesModel) }, Cmd.map ArticlesMsg articlesCmd

        | _ -> model, Cmd.none

    | ArticleMsg articleMsg ->
        match model.ActivePage with
        | Article articleModel ->
            let articleModel, articleCmd = Pages.Article.update articleMsg articleModel
            { model with ActivePage = Article articleModel }, Cmd.map ArticleMsg articleCmd

        | _ -> model, Cmd.none

    | LoginMsg loginMsg ->
        match model.ActivePage with
        | Login loginModel ->

            let loginModel, loginCmd, externalMsg = Pages.Login.update loginMsg loginModel

            let (model, cmd) =
                match externalMsg with
                | Pages.Login.ExternalMsg.NoOp -> model, Cmd.none

                | Pages.Login.ExternalMsg.SignedIn session ->
                    { model with Session = Some session },
                    Cmd.batch
                        [ Route.Article ArticlesList |> newUrl
                          saveSession session ]

            { model with ActivePage = Login loginModel },
            Cmd.batch
                [ Cmd.map LoginMsg loginCmd
                  cmd ]

        | _ -> model, Cmd.none

    | RegisterMsg registerMsg ->
        match model.ActivePage with
        | Register registerModel ->
            let registerModel, registerCmd, externalMsg = Pages.Register.update registerMsg registerModel

            let (model, cmd) =
                match externalMsg with
                | Pages.Register.ExternalMsg.NoOp -> model, Cmd.none

                | Pages.Register.ExternalMsg.UserCreated session ->
                    { model with Session = Some session },
                    Cmd.batch
                        [ Route.Article ArticlesList |> newUrl
                          saveSession session ]

            { model with ActivePage = Register registerModel },
            Cmd.batch
                [ Cmd.map RegisterMsg registerCmd
                  cmd ]

        | _ -> model, Cmd.none

    | SettingsMsg settingsMsg ->
        match model.ActivePage with
        | Settings settingsModel ->
            let settingsModel, settingsCmd = Pages.Settings.update settingsMsg settingsModel
            { model with ActivePage = Settings settingsModel }, Cmd.map SettingsMsg settingsCmd

        | _ -> model, Cmd.none

    | NewPostMsg newPostMsg ->
        match model.ActivePage with
        | NewPost newPostModel ->
            let newPostModel, newPostCmd = Pages.NewPost.update newPostMsg newPostModel
            { model with ActivePage = NewPost newPostModel }, Cmd.map NewPostMsg newPostCmd

        | _ -> model, Cmd.none

    | NoOp -> model, Cmd.none
