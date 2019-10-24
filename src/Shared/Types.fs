module Types

open System
open Thoth.Json

open Validation

type Author =
    { Username: string
      Bio: string option
      Image: string
      Following: bool }

    static member Decoder =
        Decode.object <| fun get ->
            { Username = get.Required.Field "username" Decode.string
              Bio = get.Optional.Field "bio" Decode.string
              Image = get.Required.Field "image" Decode.string
              Following = get.Required.Field "following" Decode.bool }


type Session =
    { Username: string
      Token: string }

    static member Decoder: Decoder<Session> =
        Decode.object <| fun get ->
            { Username = get.Required.Field "username" Decode.string
              Token = get.Required.Field "token" Decode.string }


type User =
    { Username: string
      Email: string
      Bio: string option
      Image: string option }

    static member Decoder: Decoder<User> =
        Decode.object <| fun get ->
            { Username = get.Required.Field "username" Decode.string
              Email = get.Required.Field "email" Decode.string
              Bio = get.Optional.Field "bio" Decode.string
              Image = get.Optional.Field "image" Decode.string }


type Article =
    { Slug: string
      Title: string
      Description: string
      Body: string
      TagList: string list
      CreatedAt: DateTime
      UpdatedAt: DateTime
      Favorited: bool
      FavoritesCount: int
      Author: Author }

    static member Decoder: Decoder<Article> =
        Decode.object <| fun get ->
            { Slug = get.Required.Field "slug" Decode.string
              Title = get.Required.Field "title" Decode.string
              Description = get.Required.Field "description" Decode.string
              Body = get.Required.Field "body" Decode.string
              TagList = get.Required.Field "tagList" (Decode.list Decode.string)
              CreatedAt = get.Required.Field "createdAt" Decode.datetime
              UpdatedAt = get.Required.Field "updatedAt" Decode.datetime
              Favorited = get.Required.Field "favorited" Decode.bool
              FavoritesCount = get.Required.Field "favoritesCount" Decode.int
              Author = get.Required.Field "author" Author.Decoder }

module User =

    type ValidatedUser = private ValidatedUser of User

    let validatedToJsonValue (ValidatedUser user) (password: string): Thoth.Json.JsonValue =
        Encode.object
            [ "username", Encode.string user.Username
              "email", Encode.string user.Email
              "bio", Option.map Encode.string user.Bio |> Option.defaultValue Encode.nil
              "image", Option.map Encode.string user.Image |> Option.defaultValue Encode.nil
              "password",
              (if String.IsNullOrWhiteSpace password then Encode.nil
               else Encode.string password) ]

    let validateUser (user: User) =
        let isUsernameEmpty user = isEmpty "username can't be blank" (fun u -> u.Username) user

        let isEmailEmpty user = isEmpty "email can't be blank" (fun u -> u.Email) user

        let isValidEmail user =
            if String.exists (fun c -> c = '@') user.Email then Ok user
            else Error "email must have a '@'"

        user
        |> isUsernameEmpty
        |> Result.bind isEmailEmpty
        |> Result.bind isValidEmail
        |> Result.map ValidatedUser


type Tag =
    | Tag of string
    static member Decoder: Decoder<Tag> = Decode.object <| fun get -> Tag <| get.Required.Raw Decode.string
    static member ListDecoder: Decoder<Tag list> =
        Decode.object (fun get -> get.Required.At [ "tags" ] (Decode.list Tag.Decoder))

module Article =

    type SimplifiedArticle =
        { Title: string
          Description: string
          Body: string
          TagList: Set<string> }

    type ValidatedSimplifiedArticle = private ValidatedSimplifiedArticle of SimplifiedArticle

    let validateArticle (simplifiedArticle: SimplifiedArticle) =
        let isTitleEmpty = isEmpty "title can't be empty" (fun a -> a.Title)

        let isDescriptionEmpty = isEmpty "description can't be empty" (fun a -> a.Body)

        let isBodyEmpty = isEmpty "body can't be empty" (fun a -> a.Description)

        simplifiedArticle
        |> isTitleEmpty
        |> Result.bind isDescriptionEmpty
        |> Result.bind isBodyEmpty
        |> Result.map ValidatedSimplifiedArticle

    let validatedToJson (ValidatedSimplifiedArticle article) =
        Encode.object
            [ "title", Encode.string article.Title
              "description", Encode.string article.Description
              "body", Encode.string article.Body
              "tagList",
              Encode.list
                  (article.TagList
                   |> Set.toList
                   |> List.map Encode.string) ]


    type ArticlesList =
        { Articles: Article list
          ArticlesCount: int }

        static member Decoder: Decoder<ArticlesList> =
            Decode.object <| fun get ->
                { Articles = get.Required.Field "articles" (Decode.list Article.Decoder)
                  ArticlesCount = get.Required.Field "articlesCount" Decode.int }


type Comment =
    { Id: int
      CreatedAt: DateTime
      UpdatedAt: DateTime
      Body: string
      Author: Author }

    static member Decoder: Decoder<Comment> =
        Decode.object <| fun get ->
            { Id = get.Required.Field "id" Decode.int
              CreatedAt = get.Required.Field "createdAt" Decode.datetime
              UpdatedAt = get.Required.Field "updatedAt" Decode.datetime
              Body = get.Required.Field "body" Decode.string
              Author = get.Required.Field "author" Author.Decoder }

    static member DecoderList: Decoder<Comment list> =
        Decode.object <| fun get -> get.Required.At [ "comments" ] (Decode.list Comment.Decoder)

    static member Encode comment = Encode.object [ ("body", Encode.string comment.Body) ]

type Profile =
    { Username: string
      Bio: string option
      Image: string
      Following: bool }

    static member Decoder: Decoder<Profile> =
        Decode.object <| fun get ->
            { Username = get.Required.Field "username" Decode.string
              Bio = get.Optional.Field "bio" Decode.string
              Image = get.Required.Field "image" Decode.string
              Following = get.Required.Field "following" Decode.bool }
