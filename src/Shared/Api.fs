module Shared.Api

open Fetch
open Thoth.Json
open Fable.RemoteData

open Shared.Types

let private baseUrl = "https://conduit.productionready.io/api/"

module Articles =

    let articlesBaseUrl = sprintf "%sarticles/" baseUrl

    let fetchArticles offset =
        promise {
            let url = sprintf "%s?limit=10&offset=%i" articlesBaseUrl offset
            let! response = fetch url []
            let! txt = response.text()
            let result = Decode.Auto.fromString<ArticlesList> (txt, isCamelCase = true)
            match result with
            | Ok data -> return Success data
            | Error err -> return Failure <| exn err
        }

    let fetchArticle slug =
        promise {
            let url = sprintf "%s/%s" articlesBaseUrl slug
            let! response = fetch url []
            let! txt = response.text()
            let result = Decode.fromString Article.Decoder txt
            match result with
            | Ok data -> return Success data
            | Error err -> return Failure <| exn err
        }

    let fetchComments slug =
        promise {
            let url = sprintf "%s/%s/comments" articlesBaseUrl slug
            let! response = fetch url []
            let! txt = response.text()
            let result = Decode.fromString Comment.DecoderList txt
            match result with
            | Ok data -> return Success data
            | Error err -> return Failure <| exn err
        }

module Tags =

    let fetchTags() =
        promise {
            let url = sprintf "%stags" baseUrl
            let! response = fetch url []
            let! txt = response.text()
            let result = Decode.fromString Tag.ListDecoder txt
            match result with
            | Ok data -> return Success data
            | Error err -> return Failure <| exn err
        }
