module Shared.Router

open Elmish.UrlParser
open Elmish.Navigation
open Fable.React.Props

type ArticleRoute =
    | ArticlesList
    | Article of string

type Route =
    | Login
    | Register
    | Article of ArticleRoute

let pageParser: Parser<Route -> Route, Route> =
    oneOf
        [ map (ArticleRoute.Article >> Article) (s "article" </> str)
          map (ArticlesList |> Article) top
          map (Login) (s "login")
          map (Register) (s "register") ]

let toHash route =
    match route with
    | (Article ArticlesList) -> ""
    | Article(ArticleRoute.Article slug) -> sprintf "article/%s" slug
    | Login -> "login"
    | Register -> "register"
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
