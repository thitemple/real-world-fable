module Shared.Types

open System
open Thoth.Json

type User =
    { Id: int
      Username: string
      Email: string
      Bio: string option
      Image: string option }

    static member Decoder: Decoder<User> =
        Decode.object <| fun get ->
            { Id = get.Required.At [ "user"; "id" ] Decode.int
              Username = get.Required.At [ "user"; "username" ] Decode.string
              Email = get.Required.At [ "user"; "email" ] Decode.string
              Bio = get.Optional.At [ "user"; "bio" ] Decode.string
              Image = get.Optional.At [ "user"; "image" ] Decode.string }

type Session =
    { Username: string
      Token: string }

    static member Decoder: Decoder<Session> =
        Decode.oneOf
            [ Decode.object <| fun get ->
                { Username = get.Required.At [ "user"; "username" ] Decode.string
                  Token = get.Required.At [ "user"; "token" ] Decode.string }
              Decode.object <| fun get ->
                  { Username = get.Required.Field "username" Decode.string
                    Token = get.Required.Field "token" Decode.string } ]

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

type Tag =
    | Tag of string
    static member Decoder: Decoder<Tag> = Decode.object <| fun get -> Tag <| get.Required.Raw Decode.string
    static member ListDecoder: Decoder<Tag list> =
        Decode.object (fun get -> get.Required.At [ "tags" ] (Decode.list Tag.Decoder))

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
            { Slug = get.Required.At [ "article"; "slug" ] Decode.string
              Title = get.Required.At [ "article"; "title" ] Decode.string
              Description = get.Required.At [ "article"; "description" ] Decode.string
              Body = get.Required.At [ "article"; "body" ] Decode.string
              TagList = get.Required.At [ "article"; "tagList" ] (Decode.list Decode.string)
              CreatedAt = get.Required.At [ "article"; "createdAt" ] Decode.datetime
              UpdatedAt = get.Required.At [ "article"; "updatedAt" ] Decode.datetime
              Favorited = get.Required.At [ "article"; "favorited" ] Decode.bool
              FavoritesCount = get.Required.At [ "article"; "favoritesCount" ] Decode.int
              Author = get.Required.At [ "article"; "author" ] Author.Decoder }

type ArticlesList =
    { Articles: Article list
      ArticlesCount: int }

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
