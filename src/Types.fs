module Types

open Fable.RemoteData

open System

type Author = {
    Username : string
    Bio : string option
    Image : string
    Following : bool
}

type Article = {
    Slug : string
    Title : string
    Description : string
    Body : string
    TagList :string list
    CreatedAt : DateTime
    UpdatedAt : DateTime
    Favorited : bool
    FavoritesCount : int
    Author : Author
}

type ArticlesResponse = {
    Articles : Article list
    ArticlesCount : int
}

type Model = {
    Articles : RemoteData<exn, Article list>
}

type Msg =
    | ArticlesFetched of RemoteData<exn, Article list>