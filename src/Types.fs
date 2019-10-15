module Types

open Fable.RemoteData
open Thoth.Json

open System

type Author = {
    Username : string
    Bio : string option
    Image : string
    Following : bool
}

type Tag =
    | Tag of string
    static member Decoder : Decoder<Tag> =
        Decode.object
            (fun get ->
                Tag <| get.Required.Raw Decode.string
            )
    static member ListDecoder : Decoder<Tag list> =
        Decode.object
            (fun get ->
                get.Required.At ["tags"] (Decode.list Tag.Decoder)
            )

type Article = {
    Slug : string
    Title : string
    Description : string
    Body : string
    TagList : string list
    CreatedAt : DateTime
    UpdatedAt : DateTime
    Favorited : bool
    FavoritesCount : int
    Author : Author
}

type ArticlesList = {
    Articles : Article list
    ArticlesCount : int
}

type Model = {
    Articles : RemoteData<exn, ArticlesList>
    PopularTags : RemoteData<exn, Tag list>
    CurrentArticlesPage : int
}

type Msg =
    | ArticlesFetched of articles: RemoteData<exn, ArticlesList>
    | TagsFetched of articles: RemoteData<exn, Tag list>
    | SetArticlesPage of int