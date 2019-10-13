module State

open Fetch
open Thoth

open Elmish
open Fable.RemoteData

open Types

module Cmds =
    let fetchArticles page =
        let articlesApi offset =
            promise {
                let url = sprintf "https://conduit.productionready.io/api/articles?limit=10&offset=%i" offset
                let! response = fetch url []
                let! txt = response.text()
                let result = Json.Decode.Auto.fromString<ArticlesList>(txt, isCamelCase=true)
                match result with
                | Ok data ->
                    return Success data
                | Error err ->
                    return Failure <| exn err
            }
        let offset = page - 1
        Cmd.OfPromise.either articlesApi offset ArticlesFetched (Failure >> ArticlesFetched)

let init() : Model * Cmd<Msg> =
    let initialPage = 1
    {
        Articles = Loading
        CurrentArticlesPage = initialPage
    }, Cmds.fetchArticles initialPage

let update msg model : Model * Cmd<Msg> =
    match msg with
    | ArticlesFetched data ->
        { model with Articles = data }, Cmd.none

    | SetArticlesPage page ->
        { model with CurrentArticlesPage = page }, Cmds.fetchArticles page