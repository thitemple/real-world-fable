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
    | Editor of Pages.Editor.Model
    | Profile of Pages.Profile.Model

type Msg =
    | ArticlesMsg of Pages.Articles.Msg
    | ArticleMsg of Pages.Article.Msg
    | LoginMsg of Pages.Login.Msg
    | RegisterMsg of Pages.Register.Msg
    | SettingsMsg of Pages.Settings.Msg
    | EditorMsg of Pages.Editor.Msg
    | ProfileMsg of Pages.Profile.Msg
    | NoOp

type Model =
    { CurrentRoute: Route option
      ActivePage: Page
      Session: Types.Session option }


// COMMANDS

let private saveSession (session: Session) =
    Cmd.OfFunc.perform (fun s ->
        let sessionStr = Encode.Auto.toString (0, s, isCamelCase = true)
        Browser.WebStorage.localStorage.setItem ("session", sessionStr)) session (fun _ -> NoOp)


let private removeSession =
    Cmd.OfFunc.perform (fun _ -> Browser.WebStorage.localStorage.removeItem ("session")) () (fun _ -> NoOp)


// STATE

let setSessionRoute sessionRoute model =
    match model.Session with
    | None -> model, newUrl Route.Login

    | Some session ->
        match sessionRoute with
        | Logout ->
            { model with Session = None },
            Cmd.batch
                [ newUrl Route.Articles
                  removeSession ]

        | SessionRoute.Settings ->
            let settingsModel, settingsCmd = Pages.Settings.init session
            { model with ActivePage = Settings settingsModel }, Cmd.map SettingsMsg settingsCmd

        | NewArticle ->
            let newArticleModel, newArticleCmd = Pages.Editor.initNew session
            { model with ActivePage = Editor newArticleModel }, Cmd.map EditorMsg newArticleCmd

        | EditArticle slug ->
            let editArticleModel, editArticleCmd = Pages.Editor.initEdit session slug
            { model with ActivePage = Editor editArticleModel }, Cmd.map EditorMsg editArticleCmd


let setRoute result model =
    let model = { model with CurrentRoute = result }

    match result with
    | None -> { model with ActivePage = NotFound }, Cmd.none

    | Some route ->
        match route with
        | SessionRoute sessionRoute -> setSessionRoute sessionRoute model

        | Route.Articles ->
            let articlesModel, articlesCmd = Pages.Articles.init model.Session
            { model with ActivePage = Articles articlesModel }, Cmd.map ArticlesMsg articlesCmd

        | Route.Article slug ->
            let articleModel, articleCmd = Pages.Article.init model.Session slug
            { model with ActivePage = Article articleModel }, Cmd.map ArticleMsg articleCmd

        | Route.Login ->
            let loginModel, loginCmd = Pages.Login.init()
            { model with ActivePage = Login loginModel }, Cmd.map LoginMsg loginCmd

        | Route.Register ->
            let registerModel, registerCmd = Pages.Register.init()
            { model with ActivePage = Register registerModel }, Cmd.map RegisterMsg registerCmd

        | Route.Profile username ->
            let profileModel, profileCmd = Pages.Profile.init model.Session username
            { model with ActivePage = Profile profileModel }, Cmd.map ProfileMsg profileCmd


let init session (route: Route option): Model * Cmd<Msg> =
    { CurrentRoute = None
      ActivePage = Loading
      Session = session }
    |> setRoute route


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
                        [ Route.Articles |> newUrl
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
                        [ Route.Articles |> newUrl
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

    | EditorMsg newPostMsg ->
        match model.ActivePage with
        | Editor newPostModel ->
            let newPostModel, newPostCmd = Pages.Editor.update newPostMsg newPostModel
            { model with ActivePage = Editor newPostModel }, Cmd.map EditorMsg newPostCmd

        | _ -> model, Cmd.none

    | ProfileMsg profileMsg ->
        match model.ActivePage with
        | Profile profileModel ->
            let profileModel, profileCmd = Pages.Profile.update profileMsg profileModel
            { model with ActivePage = Profile profileModel }, Cmd.map ProfileMsg profileCmd

        | _ -> model, Cmd.none

    | NoOp -> model, Cmd.none
