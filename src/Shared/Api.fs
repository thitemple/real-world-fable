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

let private makeRequest method body url decoder session =
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
            let decodedUser = Decode.fromString decoder response.responseText
            match decodedUser with
            | Ok user -> return Success user

            | Error e -> return Failure [ e ]

        | 422 ->
            let decodedErrors = Decode.fromString badRequestErrorDecoder response.responseText
            match decodedErrors with
            | Ok errors -> return Failure errors

            | Error e -> return Failure [ e ]

        | _ -> return Failure [ response.responseText ]
    }

let private safeGet url decoder session = makeRequest GET None url decoder (Some session)
let private safePut (body: JsonValue) url decoder session =
    makeRequest PUT (Some <| Encode.toString 0 body) url decoder (Some session)

let private get url decoder = makeRequest GET None url decoder None
let private post url decoder body = makeRequest POST (Some <| Encode.toString 0 body) url decoder None

module Articles =

    let articlesBaseUrl = sprintf "%sarticles/" baseUrl

    let fetchArticles offset =
        let url = sprintf "%s?limit=10&offset=%i" articlesBaseUrl offset
        get url ArticlesList.Decoder

    let fetchArticle slug =
        let url = sprintf "%s/%s" articlesBaseUrl slug
        get url (Decode.field "article" Article.Decoder)

    let fetchComments slug =
        let url = sprintf "%s/%s/comments" articlesBaseUrl slug
        get url Comment.DecoderList


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

    let updateUser session (validatedUser: User.ValidatedUser) password =
        let url = sprintf "%suser/" baseUrl
        safePut (User.validatedToJsonValue validatedUser password) url (Decode.field "user" User.User.Decoder) session
