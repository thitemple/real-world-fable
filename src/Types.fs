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

type ArticlesList = {
    Articles : Article list
    ArticlesCount : int
}

type Model = {
    Articles : RemoteData<exn, ArticlesList>
    CurrentArticlesPage : int
}

type Msg =
    | ArticlesFetched of articles: RemoteData<exn, ArticlesList>
    | SetArticlesPage of int