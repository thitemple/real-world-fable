module Pages.Articles

open Elmish
open Fable.RemoteData
open Fable.React
open Fable.React.Props

open Router
open Types.Article
open Types
open Api


// TYPES

type Msg =
    | ArticlesFetched of articles: RemoteData<string list, ArticlesList>
    | TagsFetched of articles: RemoteData<string list, Tag list>
    | SetArticlesPage of int

type Model =
    { Articles: RemoteData<string list, ArticlesList>
      PopularTags: RemoteData<string list, Tag list>
      CurrentArticlesPage: int }


// COMMANDS

let private fetchArticles page =
    let offset = page - 1
    Cmd.OfAsync.perform Articles.fetchArticles offset ArticlesFetched


let private fetchTags = Cmd.OfAsync.perform Tags.fetchTags () TagsFetched


// STATE

let init() =
    { Articles = Loading
      PopularTags = Loading
      CurrentArticlesPage = 1 },
    Cmd.batch
        [ fetchArticles 1
          fetchTags ]


let update msg model: Model * Cmd<Msg> =
    match msg with
    | ArticlesFetched data -> { model with Articles = data }, Cmd.none

    | SetArticlesPage page -> { model with CurrentArticlesPage = page }, fetchArticles page

    | TagsFetched data -> { model with PopularTags = data }, Cmd.none


// VIEW

let private tags tags =
    ul [ ClassName "tag-list" ]
        (List.map (fun tag -> li [ ClassName "tag-default tag-pill tag-outline" ] [ str tag ]) tags)


let private article (article: Article) =
    div [ ClassName "article-preview" ]
        [ div [ ClassName "article-meta" ]
              [ a [ Href "#" ] [ img [ Src article.Author.Image ] ] // TODO: link to author

                div [ ClassName "info" ]
                    [ a [ ClassName "author" ] [ str article.Author.Username ]

                      span [ ClassName "date" ] [ str <| article.CreatedAt.ToLongDateString() ] ]

                button [ ClassName "btn btn-outline-primary btn-sm pull-xs-right" ]  // TODO: favorite an article
                    [ i [ ClassName "ion-heart" ] []

                      str <| sprintf " %i" article.FavoritesCount ] ]
          a
              [ ClassName "preview-link"
                href <| Article article.Slug ]
              [ h1 [] [ str article.Title ]

                p [] [ str article.Description ]

                span [] [ str "Read more..." ]

                tags article.TagList ] ]

let private tagPills tags =
    fragment [] (List.map (fun (Tag tag) -> a [ ClassName "tag-pill tag-default" ] [ str tag ]) tags)

let private sidebar dispatch popularTags =
    div [ ClassName "sidebar" ]
        [ p [] [ str "Popular Tags" ]

          div [ ClassName "tag-list" ]
              [ (match popularTags with
                 | Success tags -> tagPills tags

                 | _ -> empty) ] ]


let private pagination dispatch currentPage totalArticles =
    ul [ ClassName "pagination" ]
        (seq {
            for i in 1 .. totalArticles / 10 ->
                li
                    [ classList [ ("page-item", true)
                                  ("active", i = currentPage) ] ]
                    [ a
                        [ ClassName "page-link"
                          Href "#"
                          OnClick(fun ev ->
                              ev.preventDefault()
                              dispatch <| SetArticlesPage i) ] [ str <| sprintf "%i" i ] ]
         })


let private banner =
    div [ ClassName "banner" ]
        [ div [ ClassName "container" ]
              [ h1 [ ClassName "logo-font" ] [ str "conduit" ]

                p [] [ str "A place to share your knowledge." ] ] ]


let private feedToggle =
    div [ ClassName "feed-toggle" ]
        [ ul [ ClassName "nav nav-pills outline-active" ]
              [ li [ ClassName "nav-item" ] [ a [ ClassName "nav-link disabled" ] [ str "Your Feed" ] ]

                li [ ClassName "nav-item" ] [ a [ ClassName "nav-link active" ] [ str "Global Feed" ] ] ] ]


let view dispatch model =
    div [ ClassName "home-page" ]
        [ banner
          div [ ClassName "container page" ]
              [ div [ ClassName "row" ]
                    [ div [ ClassName "col-md-9" ]
                          [ feedToggle
                            (match model.Articles with
                             | Success articles ->
                                 fragment []
                                     [ div [] (List.map article articles.Articles)

                                       pagination dispatch model.CurrentArticlesPage articles.ArticlesCount ]

                             | Loading -> div [] [ str "Loading..." ]

                             | Failure _ -> str <| sprintf "There was an issue fetching the articles"

                             | NotAsked -> empty) ]

                      div [ ClassName "col-md-3" ] [ sidebar dispatch model.PopularTags ] ] ] ]
