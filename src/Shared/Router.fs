module Shared.Router

open Elmish.UrlParser
open Elmish.Navigation
open Fable.React.Props

type ArticleRoute =
    | ArticlesList
    | Article of string

type Route = Article of ArticleRoute

let pageParser: Parser<Route -> Route, Route> =
    oneOf
        [ map (ArticleRoute.Article >> Article) (s "article" </> str)
          map (ArticlesList |> Article) top ]

let toHash route =
    match route with
    | (Article ArticlesList) -> ""
    | Article(ArticleRoute.Article slug) -> sprintf "article/%s" slug
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
