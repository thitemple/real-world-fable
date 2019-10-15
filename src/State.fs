module State

open Fetch
open Thoth.Json

open Elmish
open Fable.RemoteData

open Types

module Cmds =
    let baseUrl = "https://conduit.productionready.io/api/"
    let fetchArticles page =
        let articlesApi offset =
            promise {
                let url = sprintf "%sarticles?limit=10&offset=%i" baseUrl offset
                let! response = fetch url []
                let! txt = response.text()
                let result = Decode.Auto.fromString<ArticlesList>(txt, isCamelCase=true)
                match result with
                | Ok data ->
                    return Success data
                | Error err ->
                    return Failure <| exn err
            }
        let offset = page - 1
        Cmd.OfPromise.either articlesApi offset ArticlesFetched (Failure >> ArticlesFetched)

    let fetchTags =

        let tagsApi () =
            promise {
                let url = sprintf "%stags" baseUrl
                let! response = fetch url []
                let! txt = response.text()
                let result = Decode.fromString Tag.ListDecoder txt
                match result with
                | Ok data ->
                    return Success data
                | Error err ->
                    return Failure <| exn err
            }
        Cmd.OfPromise.either tagsApi () TagsFetched (Failure >> TagsFetched)

let init() : Model * Cmd<Msg> =
    let initialPage = 1
    {
        Articles = Loading
        PopularTags = Loading
        CurrentArticlesPage = initialPage
    }, Cmd.batch [
        Cmds.fetchArticles initialPage
        Cmds.fetchTags
    ]

let update msg model : Model * Cmd<Msg> =
    match msg with
    | ArticlesFetched data ->
        { model with Articles = data }, Cmd.none

    | SetArticlesPage page ->
        { model with CurrentArticlesPage = page }, Cmds.fetchArticles page

    | TagsFetched data ->
        { model with PopularTags = data }, Cmd.none