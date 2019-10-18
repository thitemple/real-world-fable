module RootView

open Fable.React
open Fable.React.Props

open Router
open State

let private isActiveRoute optRoute targetRoute =
    Option.map (fun route -> route = targetRoute) optRoute |> Option.defaultValue false

let authenticatedMenuItems isActiveRoute =
    fragment []
        [ li [ ClassName "nav-item" ]
              [ a
                  [ classList
                      [ ("nav-link", true)
                        ("active", isActiveRoute <| SessionRoute SessionRoute.NewArticle) ]
                    href <| SessionRoute SessionRoute.NewArticle ]
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

let private unauthenticatedMenuItems isActiveRoute =
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
                    href Route.Register ] [ str "Sign up" ] ] ]

let private navbarItems isActiveRoute session =
    match session with
    | Some _ -> authenticatedMenuItems isActiveRoute
    | None -> unauthenticatedMenuItems isActiveRoute

let private navbar isActiveRoute session =
    nav [ ClassName "navbar navbar-light" ]
        [ div [ ClassName "container" ]
              [ a
                  [ ClassName "navbar-brand"
                    href Route.Articles ] [ str "conduit" ]

                ul [ ClassName "nav navbar-nav pull-xs-right" ]
                    [ li [ ClassName "nav-item" ]
                          [ a
                              [ ClassName "nav-link active"
                                classList
                                    [ ("nav-link", true)
                                      ("active", isActiveRoute Route.Articles) ]
                                href Route.Articles ] [ str "Home" ] ]

                      navbarItems isActiveRoute session ] ] ]

let private activePage dispatch activePage =
    match activePage with
    | Articles articlesModel -> Pages.Articles.view (ArticlesMsg >> dispatch) articlesModel

    | Article articleModel -> Pages.Article.view (ArticleMsg >> dispatch) articleModel

    | Login loginModel -> Pages.Login.view (LoginMsg >> dispatch) loginModel

    | Register registerModel -> Pages.Register.view (RegisterMsg >> dispatch) registerModel

    | Settings settingsModel -> Pages.Settings.view (SettingsMsg >> dispatch) settingsModel

    | Editor editorModel -> Pages.Editor.view (EditorMsg >> dispatch) editorModel

    | Loading -> div [] [ str "Loading" ]

    | NotFound -> div [] [ str "404" ]

let rootView model dispatch =
    div []
        [ navbar (isActiveRoute model.CurrentRoute) model.Session
          activePage dispatch model.ActivePage ]
