module State

open Fetch
open Thoth

open Elmish
open Fable.RemoteData

open Types

let fetchArticles offset () =
    promise {
        let url = sprintf "https://conduit.productionready.io/api/articles?limit=10&offset=%i" offset
        let! response = fetch url []
        let! txt = response.text()
        let result = Json.Decode.Auto.fromString<ArticlesResponse>(txt, isCamelCase=true)
        match result with
        | Ok data ->
            return Success data.Articles
        | Error err ->
            return Failure <| exn err
    }

let init() : Model * Cmd<Msg> =
    { Articles = Loading }, Cmd.OfPromise.either (fetchArticles 0) () ArticlesFetched (Failure >> ArticlesFetched)

let update msg model : Model * Cmd<Msg> =
    match msg with
    | ArticlesFetched data ->
        { model with Articles = data }, Cmd.none