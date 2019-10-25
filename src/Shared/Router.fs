module Router

open Elmish.UrlParser
open Elmish.Navigation
open Fable.React.Props

type SessionRoute =
    | Settings
    | NewArticle
    | EditArticle of string
    | Profile of string
    | Logout

type Route =
    | Login
    | Register
    | Article of string
    | Articles
    | SessionRoute of SessionRoute


let pageParser: Parser<Route -> Route, Route> =
    oneOf
        [ map Article (s "article" </> str)
          map Articles top
          map Login (s "login")
          map Register (s "register")
          map (Settings |> SessionRoute) (s "settings")
          map (NewArticle |> SessionRoute) (s "editor")
          map (EditArticle >> SessionRoute) (s "editor" </> str)
          map (Profile >> SessionRoute) (s "profile" </> str)
          map (Logout |> SessionRoute) (s "logout") ]


let toHash route =
    match route with
    | Articles -> ""

    | Article slug -> sprintf "article/%s" slug

    | Login -> "login"

    | Register -> "register"

    | SessionRoute Settings -> "settings"

    | SessionRoute NewArticle -> "editor"

    | SessionRoute(Profile username) -> sprintf "profile/%s" username

    | SessionRoute Logout -> "logout"

    | SessionRoute(EditArticle slug) -> sprintf "editor/%s" slug
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
