module Shared.Api

open Thoth.Json
open Fable.RemoteData
open Fable.SimpleHttp

open Shared.Types

let private baseUrl = "https://conduit.productionready.io/api/"

let private post url data =

    let data = Encode.toString 0 data

    async {
        let! response = Http.request url
                        |> Http.method POST
                        |> Http.content (BodyContent.Text data)
                        |> Http.header (Headers.contentType "application/json")
                        |> Http.send
        return response.statusCode, response.responseText
    }


module Articles =

    let articlesBaseUrl = sprintf "%sarticles/" baseUrl

    let fetchArticles offset =
        async {
            let! (statusCode, responseText) = Http.get <| sprintf "%s?limit=10&offset=%i" articlesBaseUrl offset
            if statusCode = 200 then
                let result = Decode.Auto.fromString<ArticlesList> (responseText, isCamelCase = true)
                match result with
                | Ok data -> return Success data
                | Error err -> return Failure <| exn err
            else
                return Failure <| exn responseText
        }

    let fetchArticle slug =
        async {
            let! (statusCode, responseText) = Http.get <| sprintf "%s/%s" articlesBaseUrl slug
            if statusCode = 200 then
                let result = Decode.fromString Article.Decoder responseText
                match result with
                | Ok data -> return Success data
                | Error err -> return Failure <| exn err
            else
                return Failure <| exn responseText
        }

    let fetchComments slug =
        async {
            let! (statusCode, responseText) = Http.get <| sprintf "%s/%s/comments" articlesBaseUrl slug
            if statusCode = 200 then
                let result = Decode.fromString Comment.DecoderList responseText
                match result with
                | Ok data -> return Success data
                | Error err -> return Failure <| exn err
            else
                return Failure <| exn responseText
        }

module Tags =

    let fetchTags() =
        async {
            let! (statusCode, responseText) = Http.get <| sprintf "%stags" baseUrl
            if statusCode = 200 then
                let result = Decode.fromString Tag.ListDecoder responseText
                match result with
                | Ok data -> return Success data
                | Error err -> return Failure <| exn err
            else
                return Failure <| exn responseText
        }

module Users =

    let usersBaseUrl = sprintf "%susers/" baseUrl

    let createUser (createUser: {| username: string; email: string; password: string |}) =
        async {
            let! (statusCode, responseText) = post usersBaseUrl {| user = createUser |}

            if statusCode >= 400 then
                let problems =
                    Decode.fromString (Decode.at [ "errors" ] (Decode.dict (Decode.list Decode.string))) responseText
                match problems with
                | Ok p -> return Failure p
                | Error e -> return Failure(Map.ofList [ "Decoding error", [ e ] ])
            else
                let decodedUser = Decode.fromString User.Decoder responseText
                let decodedSession = Decode.fromString (Session.Decoder createUser.username) responseText
                match decodedUser, decodedSession with
                | Ok user, Ok session -> return Success(user, session)
                | Error userError, _ -> return Failure(Map.ofList [ "Decoding error", [ userError ] ])
                | _, Error sessionError -> return Failure(Map.ofList [ "Decoding error", [ sessionError ] ])
        }
