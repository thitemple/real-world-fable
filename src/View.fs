module View

open Fable.React
open Fable.React.Props
open Fable.RemoteData

open Types

let navbar =
    nav
        [ ClassName "navbar navbar-light" ]
        [
            div [ ClassName "container" ] [
                a [ ClassName "navbar-brand" ] [ str "conduit" ]
                ul [ ClassName "nav navbar-nav pull-xs-right" ] [
                    li [ ClassName "nav-item" ] [
                        a [ ClassName "nav-link active" ] [ str "Home" ]
                    ]
                    li [ ClassName "nav-item" ] [
                        a [ ClassName "nav-link" ] [
                            i [ ClassName "ion-compose" ] []
                            str " New Post"
                        ]
                    ]
                    li [ ClassName "nav-item" ] [
                        a [ ClassName "nav-link" ] [
                            i [ ClassName "ion-gear-a" ] []
                            str " Settings"
                        ]
                    ]
                    li [ ClassName "nav-item" ] [
                        a [ ClassName "nav-link" ] [
                            str "Sign up"
                        ]
                    ]
                ]
            ]
        ]

let article (article : Article) =
    div
        [ ClassName "article-preview" ]
        [
            div
                [ ClassName "article-meta" ]
                [
                    a
                        [ Href "#" ]
                        [
                            img [ Src article.Author.Image ]
                        ]
                    div
                        [ ClassName "info" ]
                        [
                            a [ ClassName "author" ] [ str article.Author.Username ]
                            span [ ClassName "date" ] [ str <| article.CreatedAt.ToLongDateString() ]
                        ]
                    button
                        [ ClassName "btn btn-outline-primary btn-sm pull-xs-right" ]
                        [
                            i [ ClassName "ion-heart" ] []
                            str <| sprintf " %i" article.FavoritesCount
                        ]
                ]
            a
                [ ClassName "preview-link" ]
                [
                    h1 [] [ str article.Title ]
                    p [] [ str article.Description ]
                    span [] [ str "Read more..." ]
                    ul
                        [ ClassName "tag-list" ]
                        (List.map
                            (fun tag -> li [ ClassName "tag-default tag-pill tag-outline" ] [ str tag ])
                            article.TagList
                        )
                ]
        ]

let sidebar dispatch popularTags =
    div
        [ ClassName "sidebar" ]
        [
            p [] [ str "Popular Tags" ]
            div
                [ ClassName "tag-list" ]
                [
                    (match popularTags with
                    | Success tags ->
                        fragment []
                            (
                                List.map
                                    (fun (Tag tag) ->
                                        a [ ClassName "tag-pill tag-default" ] [ str tag ]
                                    )
                                    tags
                            )
                    | _ -> str ""
                    )
                ]
        ]

let pagination dispatch currentPage totalArticles =
    ul
        [ ClassName "pagination" ]
        (
            seq { for i in 1 .. totalArticles / 10 ->
                    li
                        [
                            classList [
                                ("page-item", true)
                                ("active", i = currentPage)
                            ]
                        ]
                        [
                            a [
                                ClassName "page-link"
                                Href "#"
                                OnClick (fun ev ->
                                    ev.preventDefault()
                                    dispatch <| SetArticlesPage i
                                )
                            ] [ str <| sprintf "%i" i ]
                        ]
            }
        )


let home dispatch model =
    div
        [ ClassName "home-page" ]
        [
            div
                [ ClassName "banner" ]
                [
                    div
                        [ ClassName "container" ]
                        [
                            h1 [ ClassName "logo-font" ] [ str "conduit" ]
                            p [] [ str "A place to share your knowledge." ]
                        ]
                ]
            div
                [ ClassName "container page" ]
                [
                    div
                        [ ClassName "row" ]
                        [
                            div
                                [ ClassName "col-md-9" ]
                                [
                                    div
                                        [ ClassName "feed-toggle" ]
                                        [
                                            ul
                                                [ ClassName "nav nav-pills outline-active" ]
                                                [
                                                    li
                                                        [ ClassName "nav-item" ]
                                                        [
                                                            a [ ClassName "nav-link disabled" ] [ str "Your Feed" ]
                                                        ]
                                                    li
                                                        [ ClassName "nav-item" ]
                                                        [
                                                            a [ ClassName "nav-link active" ] [ str "Global Feed" ]
                                                        ]
                                                ]
                                        ]
                                    (
                                        match model.Articles with
                                        | Success articles ->
                                            fragment []
                                                [
                                                    div
                                                        []
                                                        (List.map article articles.Articles)
                                                    pagination dispatch model.CurrentArticlesPage articles.ArticlesCount
                                                ]
                                        | Loading ->
                                            div [] [ str "Loading..." ]
                                        | Failure _ ->
                                            str <| sprintf "There was an issue fetching the articles"
                                        | NotAsked ->
                                            str ""
                                    )
                                ]
                            div
                                [ ClassName "col-md-3" ]
                                [
                                    sidebar dispatch model.PopularTags
                                ]
                        ]
                ]
        ]

let rootView (model : Model) dispatch =
    div
        []
        [
            navbar
            home dispatch model
        ]