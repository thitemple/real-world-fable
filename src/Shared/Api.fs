module Api

open Thoth.Json
open Fable.RemoteData
open Fable.SimpleHttp

open Types

let private baseUrl = "https://conduit.productionready.io/api/"


let private badRequestErrorDecoder str =
    str
    |> Decode.at [ "errors" ]
           (Decode.dict (Decode.list Decode.string)
            |> Decode.andThen
                (Map.toList
                 >> List.collect (fun (key, errors) -> List.map (fun e -> sprintf "%s %s" key e) errors)
                 >> Decode.succeed))
// The errors are returned as a key/pair value of string * string list
// So converting all errors as just a simple string list


let private makeRequest method url decoder session body =
    async {
        let request = Http.request url |> Http.method method

        let request =
            session
            |> Option.map (fun s -> Http.header (Headers.authorization <| sprintf "Token %s" s.Token) request)
            |> Option.defaultValue request

        let request =
            body
            |> Option.map
                (fun b ->
                Http.content (BodyContent.Text b) request |> Http.header (Headers.contentType "application/json"))
            |> Option.defaultValue request

        let! response = Http.send request

        match response.statusCode with
        | 200 ->
            let decodedValue = Decode.fromString decoder response.responseText
            match decodedValue with
            | Ok value -> return Success value

            | Error e -> return Failure [ e ]

        | 422 ->
            let decodedErrors = Decode.fromString badRequestErrorDecoder response.responseText
            match decodedErrors with
            | Ok errors -> return Failure errors

            | Error e -> return Failure [ e ]

        | _ -> return Failure [ response.responseText ]
    }


let private safeGet url decoder session = makeRequest GET url decoder (Some session) None


let private safeDelete url decoder session = makeRequest DELETE url decoder (Some session) None


let private safeChange method (body: JsonValue) url decoder session =
    Some(Encode.toString 0 body) |> makeRequest method url decoder (Some session)


let private safePut url decoder session (body: JsonValue) = safeChange PUT body url decoder session


let private safePost url decoder session (body: JsonValue) = safeChange POST body url decoder session


let private get url decoder = makeRequest GET url decoder None None


let private post url decoder body = Some(Encode.toString 0 body) |> makeRequest POST url decoder None

module Articles =

    let articlesBaseUrl = sprintf "%sarticles/" baseUrl


    let fetchArticlesWithTag (payload: {| Tag: Tag; Offset: int |}) =
        let (Tag tag) = payload.Tag
        let url = sprintf "%s?tag=%s&limit=10&offset=%i" articlesBaseUrl tag payload.Offset
        get url Article.ArticlesList.Decoder


    let fetchArticles offset =
        let url = sprintf "%s?limit=10&offset=%i" articlesBaseUrl offset
        get url Article.ArticlesList.Decoder


    let fetchArticle slug =
        let url = sprintf "%s/%s" articlesBaseUrl slug
        get url (Decode.field "article" Article.Article.Decoder)


    let fetchFeed (payload: {| Session: Session; Offset: int |}) =
        let url = sprintf "%sfeed?limit=10&offset=%i" articlesBaseUrl payload.Offset
        safeGet url Article.ArticlesList.Decoder payload.Session


    let fetchComments slug =
        let url = sprintf "%s/%s/comments" articlesBaseUrl slug
        get url Comment.DecoderList


    let createArticle session (article: Article.ValidatedSimplifiedArticle) =
        Article.validatedToJson article
        |> safePost articlesBaseUrl (Decode.field "article" Article.Article.Decoder) session


    let updateArticle session (slug, article: Article.ValidatedSimplifiedArticle) =
        let url = sprintf "%s/%s" articlesBaseUrl slug
        Article.validatedToJson article |> safePut url (Decode.field "article" Article.Article.Decoder) session


    let createComment (payload: {| Session: Session; Slug: string; CommentBody: string |}) =
        let url = sprintf "%s/%s/comments" articlesBaseUrl payload.Slug
        let comment = Encode.object [ ("body", Encode.string payload.CommentBody) ]
        safePost url (Decode.field "comment" Comment.Decoder) payload.Session {| comment = comment |}


    let favoriteArticle (payload: {| Session: Session; Article: Article.Article |}) =
        let url = sprintf "%s/%s/favorite" articlesBaseUrl payload.Article.Slug
        safePost url (Decode.field "article" Article.Article.Decoder) payload.Session ""


    let unfavoriteArticle (payload: {| Session: Session; Article: Article.Article |}) =
        let url = sprintf "%s/%s/favorite" articlesBaseUrl payload.Article.Slug
        safeDelete url (Decode.field "article" Article.Article.Decoder) payload.Session



module Tags =

    let fetchTags() =
        let url = sprintf "%stags" baseUrl
        get url Tag.ListDecoder


module Users =

    let usersBaseUrl = sprintf "%susers/" baseUrl


    let createUser (createUser: {| username: string; email: string; password: string |}) =
        post usersBaseUrl (Decode.field "user" Session.Decoder) {| user = createUser |}


    let login (credentials: {| email: string; password: string |}) =
        let url = sprintf "%slogin/" usersBaseUrl
        post url (Decode.field "user" Session.Decoder) {| user = credentials |}


    let fetchUserWithDecoder decoder session =
        let url = sprintf "%suser/" baseUrl
        safeGet url (Decode.field "user" decoder) session


    let fetchUser session = fetchUserWithDecoder User.User.Decoder session


    let updateUser session (validatedUser: User.ValidatedUser, password) =
        let url = sprintf "%suser/" baseUrl
        User.validatedToJsonValue validatedUser password |> safePut url (Decode.field "user" User.User.Decoder) session
