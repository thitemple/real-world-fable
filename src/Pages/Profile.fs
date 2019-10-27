module Pages.Profile

open Elmish
open Fable.React
open Fable.React.Props
open Fable.RemoteData

open Types
open Types.Article
open Router
open Api


// TYPES

type ArticlesView =
    | AuthorArticles
    | FavoritedArticles

type Model =
    { Author: RemoteData<string list, Author>
      Username: string
      ArticlesView: ArticlesView
      Articles: RemoteData<string list, ArticlesList>
      Session: Session option }

type Msg =
    | ProfileLoaded of RemoteData<string list, Author>
    | ArticlesLoaded of RemoteData<string list, ArticlesList>
    | ToggleFavoriteArticle of FullArticle
    | FavoriteArticleToggled of RemoteData<string list, FullArticle>
    | ToggleFollowAuthor of Author
    | FollowAuthorToggled of RemoteData<string list, Author>
    | ToggleArticlesView of ArticlesView


// COMMANDS

let private fetchProfile username = Cmd.OfAsync.perform Profiles.fetchProfile username ProfileLoaded


let private fetchArticlesFromAuthor username =
    Cmd.OfAsync.perform Articles.fetchArticlesFromAuthor username ArticlesLoaded


let private fetchFavoriteArticlesFromAuthor author =
    Cmd.OfAsync.perform Articles.fetchFavoriteArticles author ArticlesLoaded


let private favArticle session article =
    session
    |> Option.map (fun s ->
        Cmd.OfAsync.perform Articles.favoriteArticle
            {| Session = s
               Article = article |} FavoriteArticleToggled)
    |> Option.defaultValue Cmd.none


let private unfavArticle session article =
    session
    |> Option.map
        (fun s ->
        Cmd.OfAsync.perform Articles.unfavoriteArticle
            {| Session = s
               Article = article |} FavoriteArticleToggled)
    |> Option.defaultValue Cmd.none

let private followAuthor session author =
    session
    |> Option.map (fun s ->
        Cmd.OfAsync.perform Profiles.createFollower
            {| Session = s
               Author = author |} FollowAuthorToggled)
    |> Option.defaultValue Cmd.none

let private unfollowAuthor session author =
    session
    |> Option.map (fun s ->
        Cmd.OfAsync.perform Profiles.deleteFollower
            {| Session = s
               Author = author |} FollowAuthorToggled)
    |> Option.defaultValue Cmd.none


// STATE

let init session username =
    { Author = Loading
      Username = username
      ArticlesView = AuthorArticles
      Articles = Loading
      Session = session },
    Cmd.batch
        [ fetchProfile username
          fetchArticlesFromAuthor username ]


let update msg (model: Model) =
    match msg with
    | ProfileLoaded data -> { model with Author = data }, Cmd.none

    | ArticlesLoaded data -> { model with Articles = data }, Cmd.none

    | ToggleFavoriteArticle({ Favorited = true } as article) -> model, unfavArticle model.Session article

    | ToggleFavoriteArticle article -> model, favArticle model.Session article

    | FavoriteArticleToggled(Success article) ->
        let articles =
            map (fun (articlesList: ArticlesList) ->
                let articles =
                    List.map (fun a ->
                        if a.Slug = article.Slug then article
                        else a) articlesList.Articles
                { articlesList with Articles = articles }) model.Articles
        { model with Articles = articles }, Cmd.none

    | FavoriteArticleToggled _ -> model, Cmd.none

    | ToggleFollowAuthor({ Following = true } as author) -> model, unfollowAuthor model.Session author

    | ToggleFollowAuthor author -> model, followAuthor model.Session author

    | FollowAuthorToggled data -> { model with Author = data }, Cmd.none

    | ToggleArticlesView articlesView ->
        match articlesView, model.Author with
        | FavoritedArticles, Success author ->
            { model with ArticlesView = FavoritedArticles }, fetchFavoriteArticlesFromAuthor author

        | AuthorArticles, Success author ->
            { model with ArticlesView = AuthorArticles }, fetchArticlesFromAuthor author.Username

        | _ -> model, Cmd.none


// VIEW

let private userInfoButtons dispatch (session: Session option) (author: Author) =
    match session with
    | Some s when s.Username = author.Username ->
        a
            [ ClassName "btn btn-sm btn-outline-secondary action-btn"
              href <| SessionRoute Settings ] [ i [ ClassName "ion-gear-a" ] [ str " Edit Profile Settings" ] ]

    | _ ->
        button
            [ classList
                [ ("btn", true)
                  ("btn-sm", true)
                  ("action-btn", true)
                  ("btn-outline-secondary", not author.Following)
                  ("btn-secondary", author.Following) ]
              OnClick(fun _ -> dispatch <| ToggleFollowAuthor author) ]
            [ i [ ClassName "ion-plus-round" ] []

              str <| sprintf " %s %s"
                         (if author.Following then "Unfollow"
                          else "Follow") author.Username ]

let private userInfo dispatch session (author: Author) =
    div [ ClassName "user-info" ]
        [ div [ ClassName "container" ]
              [ div [ ClassName "row" ]
                    [ div [ ClassName "col-xs-12 col-md-10 offset-md-1" ]
                          [ img [ Src author.Image ]

                            h4 [] [ str author.Username ]

                            p [] [ str <| Option.defaultWith (fun _ -> "") author.Bio ]

                            userInfoButtons dispatch session author ] ] ] ]


let private article dispatch (article: FullArticle) =
    div [ ClassName "article-preview" ]
        [ div [ ClassName "article-meta" ]
              [ a [ href <| Profile article.Author.Username ] [ img [ Src article.Author.Image ] ]

                div [ ClassName "info" ]
                    [ a [ href <| Profile article.Author.Username ] [ str article.Author.Username ]

                      span [ ClassName "date" ] [ str <| article.CreatedAt.ToLongDateString() ] ]

                div [ ClassName "pull-xs-right" ]
                    [ button
                        [ classList
                            [ ("btn", true)
                              ("btn-sm", true)
                              ("btn-outline-primary", not article.Favorited)
                              ("btn-primary", article.Favorited) ]
                          OnClick(fun _ -> dispatch <| ToggleFavoriteArticle article) ]
                          [ i [ ClassName "ion-heart" ] []

                            str <| sprintf " %i" article.FavoritesCount ] ] ]

          a
              [ ClassName "preview-link"
                href <| Article article.Slug ]
              [ h1 [] [ str article.Title ]

                p [] [ str article.Description ]

                span [] [ str "Read more..." ] ] ]


let private articlesToggle dispatch currentArticlesView =
    div [ ClassName "articles-toggle" ]
        [ ul [ ClassName "nav nav-pills outline-active" ]
              [ li [ ClassName "nav-item" ]
                    [ a
                        [ classList
                            [ ("nav-link", true)
                              ("active", currentArticlesView = AuthorArticles) ]
                          Href ""
                          OnClick(fun ev ->
                              ev.preventDefault()
                              dispatch <| ToggleArticlesView AuthorArticles) ] [ str "My Articles" ] ]

                li [ ClassName "nav-item" ]
                    [ a
                        [ classList
                            [ ("nav-link", true)
                              ("active", currentArticlesView = FavoritedArticles) ]
                          Href ""
                          OnClick(fun ev ->
                              ev.preventDefault()
                              dispatch <| ToggleArticlesView FavoritedArticles) ] [ str "Favorited Articles" ] ] ] ]


let view dispatch model =
    div [ ClassName "profile-page" ]
        [ (match model.Author with
           | Success profile -> userInfo dispatch model.Session profile

           | Failure err ->
               div
                   [ Style [ Padding 10
                             Color "red"
                             BackgroundColor "lightpink" ] ] (List.map (fun e -> p [] [ str e ]) err)

           | _ -> empty)

          div [ ClassName "container" ]
              [ div [ ClassName "row" ]
                    [ div [ ClassName "col-xs-12 col-md-10 offset-md-1" ]
                          [ fragment []
                                [ articlesToggle dispatch model.ArticlesView

                                  (match model.Articles with
                                   | Success articles -> fragment [] (List.map (article dispatch) articles.Articles)

                                   | _ -> empty) ] ] ] ] ]
