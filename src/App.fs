module App

open Elmish
open Shared.Router

type Page =
    | NotFound
    | Loading
    | Login of Pages.Login.Model
    | Register of Pages.Register.Model
    | Articles of Pages.Articles.Model
    | Article of Pages.Article.Model

type Msg =
    | ArticlesMsg of Pages.Articles.Msg
    | ArticleMsg of Pages.Article.Msg
    | LoginMsg of Pages.Login.Msg
    | RegisterMsg of Pages.Register.Msg

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

let private init (route: Route option): Model * Cmd<Msg> =
    { CurrentRoute = None
      ActivePage = Loading
      Session = None }
    |> setRoute route

let private update msg model: Model * Cmd<Msg> =
    match msg, model.ActivePage with
    | ArticlesMsg articlesMsg, Articles articlesModel ->
        let articlesModel, articlesCmd = Pages.Articles.update articlesMsg articlesModel
        { model with ActivePage = (Articles articlesModel) }, Cmd.map ArticlesMsg articlesCmd

    | ArticleMsg articleMsg, Article articleModel ->
        let articleModel, articleCmd = Pages.Article.update articleMsg articleModel
        { model with ActivePage = Article articleModel }, Cmd.map ArticleMsg articleCmd

    | LoginMsg loginMsg, Login loginModel ->
        let loginModel, loginCmd = Pages.Login.update loginMsg loginModel
        { model with ActivePage = Login loginModel }, Cmd.map LoginMsg loginCmd

    | RegisterMsg registerMsg, Register registerModel ->
        let registerModel, registerCmd = Pages.Register.update registerMsg registerModel
        { model with ActivePage = Register registerModel }, Cmd.map RegisterMsg registerCmd

    | _ -> model, Cmd.none

open Fable.React
open Fable.React.Props

let navbar session activePage =
    nav [ ClassName "navbar navbar-light" ]
        [ div [ ClassName "container" ]
              [ a [ ClassName "navbar-brand" ] [ str "conduit" ]
                ul [ ClassName "nav navbar-nav pull-xs-right" ]
                    [ li [ ClassName "nav-item" ]
                          [ a
                              [ ClassName "nav-link active"
                                classList
                                    [ ("nav-link", true)
                                      ("active", true) ]
                                href (Route.Article ArticlesList) ] [ str "Home" ] ]
                      (match session with
                       | Some _ ->
                           fragment []
                               [ li [ ClassName "nav-item" ]
                                     [ a [ ClassName "nav-link" ]
                                           [ i [ ClassName "ion-compose" ] []
                                             str " New Post" ] ]
                                 li [ ClassName "nav-item" ]
                                     [ a [ ClassName "nav-link" ]
                                           [ i [ ClassName "ion-gear-a" ] []
                                             str " Settings" ] ] ]
                       | None ->
                           fragment []
                               [ li [ ClassName "nav-item" ]
                                     [ a
                                         [ ClassName "nav-link"
                                           href Route.Login ] [ str "Sign in" ] ]
                                 li [ ClassName "nav-item" ] [ a [ ClassName "nav-link" ] [ str "Sign up" ] ] ]) ] ] ]

let private rootView (model: Model) dispatch =
    div []
        [ navbar model.Session model.ActivePage
          (match model.ActivePage with
           | Articles articlesModel -> Pages.Articles.view (ArticlesMsg >> dispatch) articlesModel
           | Article articleModel -> Pages.Article.view (ArticleMsg >> dispatch) articleModel
           | Login loginModel -> Pages.Login.view (LoginMsg >> dispatch) loginModel
           | Register registerModel -> Pages.Register.view (RegisterMsg >> dispatch) registerModel
           | Loading -> div [] [ str "Loading" ]
           | NotFound -> div [] [ str "404" ]) ]

open Elmish.HMR
open Elmish.UrlParser
open Elmish.Navigation

Program.mkProgram init update rootView
|> Program.toNavigable (parseHash pageParser) setRoute
|> Program.withReactSynchronous "real-world-fable-app"
|> Program.withConsoleTrace
|> Program.run
