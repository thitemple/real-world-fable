module Pages.Articles

open Elmish
open Fable.React
open Fable.React.Props
open Fable.RemoteData

open Router
open Types.Article
open Types
open Api


// TYPES

type ArticlesView =
    | Feed
    | Global
    | TagFeed of Tag

type Msg =
    | ArticlesFetched of RemoteData<string list, ArticlesList>
    | TagsFetched of RemoteData<string list, Tag list>
    | ArticleFavorited of RemoteData<string list, FullArticle>
    | ArticleUnfavorited of RemoteData<string list, FullArticle>
    | SetArticlesPage of int
    | FavoriteArticle of FullArticle
    | UnfavoriteArticle of FullArticle
    | ToggleArticlesView of ArticlesView

type Model =
    { Articles: RemoteData<string list, ArticlesList>
      PopularTags: RemoteData<string list, Tag list>
      CurrentArticlesPage: int
      ArticlesView: ArticlesView
      Session: Session option }


// COMMANDS

let private fetchArticles session articlesView page =
    let offset = page - 1
    match articlesView, session with
    | Feed, Some s ->
        Cmd.OfAsync.perform Articles.fetchFeed
            {| Session = s
               Offset = offset |} ArticlesFetched

    | TagFeed tag, _ ->
        Cmd.OfAsync.perform Articles.fetchArticlesWithTag
            {| Tag = tag
               Offset = offset |} ArticlesFetched

    | _ -> Cmd.OfAsync.perform Articles.fetchArticles offset ArticlesFetched


let private fetchTags = Cmd.OfAsync.perform Tags.fetchTags () TagsFetched


let private favArticle session article =
    Cmd.OfAsync.perform Articles.favoriteArticle
        {| Session = session
           Article = article |} ArticleFavorited


let private unfavArticle session article =
    Cmd.OfAsync.perform Articles.unfavoriteArticle
        {| Session = session
           Article = article |} ArticleUnfavorited


// STATE

let init session =
    let articlesView = Option.map (fun _ -> Feed) session |> Option.defaultValue Global
    { Articles = Loading
      PopularTags = Loading
      CurrentArticlesPage = 1
      ArticlesView = articlesView
      Session = session },
    Cmd.batch
        [ fetchArticles session articlesView 1
          fetchTags ]


let update msg model: Model * Cmd<Msg> =
    match msg with
    | ArticlesFetched data -> { model with Articles = data }, Cmd.none

    | SetArticlesPage page ->
        { model with CurrentArticlesPage = page }, fetchArticles model.Session model.ArticlesView page

    | TagsFetched data -> { model with PopularTags = data }, Cmd.none

    | ArticleFavorited(Success article) ->
        map (fun (articles: ArticlesList) ->
            let updatedArticles =
                List.map (fun a ->
                    if a.Slug = article.Slug then article
                    else a) articles.Articles
            { model with Articles = Success { articles with Articles = updatedArticles } }, Cmd.none) model.Articles
        |> withDefault (model, Cmd.none)

    | ArticleFavorited _ -> model, Cmd.none

    | ArticleUnfavorited(Success article) ->
        map (fun (articles: ArticlesList) ->
            let updatedArticles =
                List.map (fun a ->
                    if a.Slug = article.Slug then article
                    else a) articles.Articles
            { model with Articles = Success { articles with Articles = updatedArticles } }, Cmd.none) model.Articles
        |> withDefault (model, Cmd.none)

    | ArticleUnfavorited _ -> model, Cmd.none

    | FavoriteArticle article ->
        match model.Session with
        | Some s -> model, favArticle s article

        | None -> model, Cmd.none

    | UnfavoriteArticle article ->
        match model.Session with
        | Some s -> model, unfavArticle s article

        | None -> model, Cmd.none

    | ToggleArticlesView articlesView ->
        { model with ArticlesView = articlesView }, fetchArticles model.Session articlesView 1


// VIEW

let private tags tags =
    ul [ ClassName "tag-list" ]
        (List.map (fun tag -> li [ ClassName "tag-default tag-pill tag-outline" ] [ str tag ]) tags)


let private article dispatch (article: FullArticle) =
    div [ ClassName "article-preview" ]
        [ div [ ClassName "article-meta" ]
              [ a [ href <| SessionRoute(Profile article.Author.Username) ] [ img [ Src article.Author.Image ] ]

                div [ ClassName "info" ]
                    [ a [ ClassName "author" ] [ str article.Author.Username ]

                      span [ ClassName "date" ] [ str <| article.CreatedAt.ToLongDateString() ] ]

                div [ ClassName "pull-xs-right" ]
                    [ button
                        [ classList
                            [ ("btn", true)
                              ("btn-sm", true)
                              ("btn-outline-primary", not article.Favorited)
                              ("btn-primary", article.Favorited) ]
                          OnClick(fun _ ->
                              if article.Favorited then dispatch <| UnfavoriteArticle article
                              else dispatch <| FavoriteArticle article) ]
                          [ i [ ClassName "ion-heart" ] []

                            str <| sprintf " %i" article.FavoritesCount ] ] ]
          a
              [ ClassName "preview-link"
                href <| Article article.Slug ]
              [ h1 [] [ str article.Title ]

                p [] [ str article.Description ]

                span [] [ str "Read more..." ]

                tags article.TagList ] ]

let private tagPills dispatch tags =
    fragment []
        (List.map (fun (Tag tag) ->
            a
                [ ClassName "tag-pill tag-default"
                  Href ""
                  OnClick(fun ev ->
                      ev.preventDefault()
                      Tag tag
                      |> TagFeed
                      |> ToggleArticlesView
                      |> dispatch) ] [ str tag ]) tags)

let private sidebar dispatch popularTags =
    div [ ClassName "sidebar" ]
        [ p [] [ str "Popular Tags" ]

          div [ ClassName "tag-list" ]
              [ (match popularTags with
                 | Success tags -> tagPills dispatch tags

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


let private feedToggle dispatch articlesView session =
    div [ ClassName "feed-toggle" ]
        [ ul [ ClassName "nav nav-pills outline-active" ]
              [ (match session with
                 | Some _ ->
                     li [ ClassName "nav-item" ]
                         [ a
                             [ classList
                                 [ ("nav-link", true)
                                   ("active", articlesView = Feed) ]
                               Href ""
                               OnClick(fun ev ->
                                   ev.preventDefault()
                                   dispatch <| ToggleArticlesView Feed) ] [ str "Your Feed" ] ]

                 | None -> empty)

                li [ ClassName "nav-item" ]
                    [ a
                        [ classList
                            [ ("nav-link", true)
                              ("active", articlesView = Global) ]
                          Href ""
                          OnClick(fun ev ->
                              ev.preventDefault()
                              dispatch <| ToggleArticlesView Global) ] [ str "Global Feed" ] ]

                (match articlesView with
                 | TagFeed(Tag t) ->
                     li [ ClassName "nav-item" ] [ a [ ClassName "nav-link active" ] [ str <| sprintf "# %s" t ] ]

                 | _ -> empty) ] ]


let view dispatch model =
    div [ ClassName "home-page" ]
        [ banner
          div [ ClassName "container page" ]
              [ div [ ClassName "row" ]
                    [ div [ ClassName "col-md-9" ]
                          [ feedToggle dispatch model.ArticlesView model.Session
                            (match model.Articles with
                             | Success({ Articles = [] }) ->
                                 div [ ClassName "article-preview" ] [ str "No articles here... yet." ]

                             | Success articles ->
                                 fragment []
                                     [ div [] (List.map (article dispatch) articles.Articles)

                                       pagination dispatch model.CurrentArticlesPage articles.ArticlesCount ]

                             | Loading -> div [] [ str "Loading..." ]

                             | Failure _ -> str <| sprintf "There was an issue fetching the articles"

                             | NotAsked -> empty) ]

                      div [ ClassName "col-md-3" ] [ sidebar dispatch model.PopularTags ] ] ] ]
