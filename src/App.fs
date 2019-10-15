module App

open Elmish
open Shared.Router

type Page =
    | NotFound
    | Loading
    | Articles of Pages.Articles.Model
    | Article of Pages.Article.Model

type Msg =
    | ArticlesMsg of Pages.Articles.Msg
    | ArticleMsg of Pages.Article.Msg

type Model =
    { CurrentRoute: Route option
      ActivePage: Page }

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

let private init (route: Route option): Model * Cmd<Msg> =
    { CurrentRoute = None
      ActivePage = Loading }
    |> setRoute route

let private update msg model: Model * Cmd<Msg> =
    match msg, model.ActivePage with
    | ArticlesMsg articlesMsg, Articles articlesModel ->
        let articlesModel, articlesCmd = Pages.Articles.update articlesMsg articlesModel
        { model with ActivePage = (Articles articlesModel) }, Cmd.map ArticlesMsg articlesCmd

    | ArticleMsg articleMsg, Article articleModel ->
        let articleModel, articleCmd = Pages.Article.update articleMsg articleModel
        { model with ActivePage = Article articleModel }, Cmd.map ArticleMsg articleCmd

    | _ -> model, Cmd.none

open Fable.React
open Fable.React.Props

let navbar =
    nav [ ClassName "navbar navbar-light" ]
        [ div [ ClassName "container" ]
              [ a [ ClassName "navbar-brand" ] [ str "conduit" ]
                ul [ ClassName "nav navbar-nav pull-xs-right" ]
                    [ li [ ClassName "nav-item" ]
                          [ a
                              [ ClassName "nav-link active"
                                href (Route.Article ArticlesList) ] [ str "Home" ] ]
                      li [ ClassName "nav-item" ]
                          [ a [ ClassName "nav-link" ]
                                [ i [ ClassName "ion-compose" ] []
                                  str " New Post" ] ]
                      li [ ClassName "nav-item" ]
                          [ a [ ClassName "nav-link" ]
                                [ i [ ClassName "ion-gear-a" ] []
                                  str " Settings" ] ]
                      li [ ClassName "nav-item" ] [ a [ ClassName "nav-link" ] [ str "Sign up" ] ] ] ] ]

let private rootView (model: Model) dispatch =
    div []
        [ navbar
          (match model.ActivePage with
           | Articles articlesModel -> Pages.Articles.view (ArticlesMsg >> dispatch) articlesModel
           | Article articleModel -> Pages.Article.view (ArticleMsg >> dispatch) articleModel
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
