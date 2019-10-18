module App

open Elmish
open Thoth.Json

open Shared.Router
open Shared.Types

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
      Session: Shared.Types.Session option }

let private setRoute result model =
    let model = { model with CurrentRoute = result }

    match result with
    | None -> { model with ActivePage = NotFound }, Cmd.none
    | Some route ->
        match route with
        | SessionRoute sessionRoute ->
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

let private init session (route: Route option): Model * Cmd<Msg> =
    { CurrentRoute = None
      ActivePage = Loading
      Session = session }
    |> setRoute route

let private update msg model: Model * Cmd<Msg> =

    let saveSession (session: Session) =
        Cmd.OfFunc.perform (fun s ->
            let sessionStr = Encode.Auto.toString (0, s, isCamelCase = true)
            Browser.WebStorage.localStorage.setItem ("session", sessionStr)) session (fun _ -> NoOp)

    match msg, model.ActivePage with
    | ArticlesMsg articlesMsg, Articles articlesModel ->
        let articlesModel, articlesCmd = Pages.Articles.update articlesMsg articlesModel
        { model with ActivePage = (Articles articlesModel) }, Cmd.map ArticlesMsg articlesCmd

    | ArticleMsg articleMsg, Article articleModel ->
        let articleModel, articleCmd = Pages.Article.update articleMsg articleModel
        { model with ActivePage = Article articleModel }, Cmd.map ArticleMsg articleCmd

    | LoginMsg loginMsg, Login loginModel ->
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

    | RegisterMsg registerMsg, Register registerModel ->
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

    | SettingsMsg settingsMsg, Settings settingsModel ->
        let settingsModel, settingsCmd = Pages.Settings.update settingsMsg settingsModel
        { model with ActivePage = Settings settingsModel }, Cmd.map SettingsMsg settingsCmd

    | NewPostMsg newPostMsg, NewPost newPostModel ->
        let newPostModel, newPostCmd = Pages.NewPost.update newPostMsg newPostModel
        { model with ActivePage = NewPost newPostModel }, Cmd.map NewPostMsg newPostCmd

    | _ -> model, Cmd.none

open Fable.React
open Fable.React.Props

let isActiveRoute optRoute targetRoute =
    Option.map (fun route -> route = targetRoute) optRoute |> Option.defaultValue false

let navbar isActiveRoute session =
    nav [ ClassName "navbar navbar-light" ]
        [ div [ ClassName "container" ]
              [ a [ ClassName "navbar-brand" ] [ str "conduit" ]
                ul [ ClassName "nav navbar-nav pull-xs-right" ]
                    [ li [ ClassName "nav-item" ]
                          [ a
                              [ ClassName "nav-link active"
                                classList
                                    [ ("nav-link", true)
                                      ("active", isActiveRoute <| Route.Article ArticlesList) ]
                                href (Route.Article ArticlesList) ] [ str "Home" ] ]
                      (match session with
                       | Some _ ->
                           fragment []
                               [ li [ ClassName "nav-item" ]
                                     [ a
                                         [ classList [ ("nav-link", true); ("active", isActiveRoute <| SessionRoute SessionRoute.NewPost) ]
                                           href <| SessionRoute SessionRoute.NewPost ]
                                           [ i [ ClassName "ion-compose" ] []
                                             str " New Post" ] ]
                                 li [ ClassName "nav-item" ]
                                     [ a
                                         [ classList
                                             [ ("nav-link", true)
                                               ("active", isActiveRoute <| SessionRoute SessionRoute.Settings) ]
                                           href <| SessionRoute SessionRoute.Settings ]
                                           [ i [ ClassName "ion-gear-a" ] []
                                             str " Settings" ] ]
                                 li [ ClassName "nav-item" ]
                                     [ a
                                         [ ClassName "nav-link"
                                           href <| SessionRoute Logout ] [ str " Sign out" ] ] ]
                       | None ->
                           fragment []
                               [ li [ ClassName "nav-item" ]
                                     [ a
                                         [ classList
                                             [ ("nav-link", true)
                                               ("active", isActiveRoute Route.Login) ]
                                           href Route.Login ] [ str "Sign in" ] ]
                                 li [ ClassName "nav-item" ]
                                     [ a
                                         [ classList
                                             [ ("nav-link", true)
                                               ("active", isActiveRoute Route.Register) ]
                                           href Route.Register ] [ str "Sign up" ] ] ]) ] ] ]

let private rootView (model: Model) dispatch =
    div []
        [ navbar (isActiveRoute model.CurrentRoute) model.Session
          (match model.ActivePage with
           | Articles articlesModel -> Pages.Articles.view (ArticlesMsg >> dispatch) articlesModel
           | Article articleModel -> Pages.Article.view (ArticleMsg >> dispatch) articleModel
           | Login loginModel -> Pages.Login.view (LoginMsg >> dispatch) loginModel
           | Register registerModel -> Pages.Register.view (RegisterMsg >> dispatch) registerModel
           | Settings settingsModel -> Pages.Settings.view (SettingsMsg >> dispatch) settingsModel
           | NewPost newPostModel -> Pages.NewPost.view (NewPostMsg >> dispatch) newPostModel
           | Loading -> div [] [ str "Loading" ]
           | NotFound -> div [] [ str "404" ]) ]

open Elmish.HMR
open Elmish.UrlParser
open Elmish.Navigation

let private tryGetSessionFromLocalStorage =
    Browser.WebStorage.localStorage.getItem "session"
    |> Option.ofObj
    |> Option.bind (fun sessionStr -> Decode.fromString Session.Decoder sessionStr |> Result.toOption)


Program.mkProgram (init tryGetSessionFromLocalStorage) update rootView
|> Program.toNavigable (parseHash pageParser) setRoute
|> Program.withReactSynchronous "real-world-fable-app"
|> Program.withConsoleTrace
|> Program.run
