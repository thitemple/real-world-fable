module Router

open Elmish.UrlParser
open Elmish.Navigation
open Fable.React.Props

type ArticleRoute =
    | ArticlesList
    | Article of string

type SessionRoute =
    | Settings
    | NewPost
    | Logout

type Route =
    | Login
    | Register
    | Article of ArticleRoute
    | SessionRoute of SessionRoute

let pageParser: Parser<Route -> Route, Route> =
    oneOf
        [ map (ArticleRoute.Article >> Article) (s "article" </> str)
          map (ArticlesList |> Article) top
          map (Login) (s "login")
          map (Register) (s "register")
          map (Settings |> SessionRoute) (s "settings")
          map (NewPost |> SessionRoute) (s "editor")
          map (Logout |> SessionRoute) (s "logout") ]

let toHash route =
    match route with
    | (Article ArticlesList) -> ""

    | Article(ArticleRoute.Article slug) -> sprintf "article/%s" slug

    | Login -> "login"

    | Register -> "register"

    | SessionRoute Settings -> "settings"

    | SessionRoute NewPost -> "editor"

    | SessionRoute Logout -> "logout"
    |> (fun r -> sprintf "#/%s" r)

let href = toHash >> Href

let modifyUrl route =
    route
    |> toHash
    |> Navigation.modifyUrl

let newUrl route =
    route
    |> toHash
    |> Navigation.newUrl
