module Pages.Profile

open Elmish
open Fable.React
open Fable.React.Props
open Fable.RemoteData

open Types
open Types.Article
open Types.User
open Api


// TYPES

type ArticlesView =
    | AuthorArticles of RemoteData<string list, ArticlesList>
    | FavoritedArticles of RemoteData<string list, ArticlesList>

type Model =
    { Profile: RemoteData<string list, Profile>
      Username: string
      ArticlesView: ArticlesView }

type Msg =
    | ProfileLoaded of RemoteData<string list, Profile>
    | ArticlesLoaded of RemoteData<string list, ArticlesList>


// COMMANDS

let private fetchProfile username = Cmd.OfAsync.perform Profiles.fetchProfile username ProfileLoaded


let private fetchArticlesFromAuthor username =
    Cmd.OfAsync.perform Articles.fetchArticlesFromAuthor username ArticlesLoaded


// STATE

let init username =
    { Profile = Loading
      Username = username
      ArticlesView = AuthorArticles Loading },
    Cmd.batch
        [ fetchProfile username
          fetchArticlesFromAuthor username ]


let update msg model =
    match msg with
    | ProfileLoaded data -> { model with Profile = data }, Cmd.none

    | ArticlesLoaded data ->
        match model.ArticlesView with
        | AuthorArticles _ -> { model with ArticlesView = AuthorArticles data }, Cmd.none

        | FavoritedArticles _ -> { model with ArticlesView = FavoritedArticles data }, Cmd.none


// VIEW

let private userInfo (profile: Profile) =
    div [ ClassName "user-info" ]
        [ div [ ClassName "container" ]
              [ div [ ClassName "row" ]
                    [ div [ ClassName "col-xs-12 col-md-10 offset-md-1" ]
                          [ img [ Src profile.Image ]

                            h4 [] [ str profile.Username ]

                            p [] [ str <| Option.defaultWith (fun _ -> "") profile.Bio ]

                            // TODO: implement follow author
                            // TODO: replace with link to edit own profile
                            button [ ClassName "btn btn-sm btn-outline-secondary action-btn" ]
                                [ i [ ClassName "ion-plus-round" ] []

                                  str <| sprintf " Follow %s" profile.Username ] ] ] ] ]


let private article (article: FullArticle) =
    div [ ClassName "article-preview" ]
        [ div [ ClassName "article-meta" ]
              [ a [] [ img [ Src article.Author.Image ] ]

                div [ ClassName "info" ]
                    [ a [] [ str article.Author.Username ]

                      span [ ClassName "date" ] [ str <| article.CreatedAt.ToLongDateString() ] ]

                button [ ClassName "btn btn-outline-primary btn-sm pull-xs-right" ]
                    [ i [ ClassName "ion-heart" ] []

                      str " 29" ] ]

          a [ ClassName "preview-link" ]
              [ h1 [] [ str article.Title ]

                p [] [ str article.Description ]

                span [] [ str "Read more..." ] ] ] // TODO: link to author and favorite count


let private articlesToggle =
    div [ ClassName "articles-toggle" ]
        [ ul [ ClassName "nav nav-pills outline-active" ]
              [ li [ ClassName "nav-item" ] [ a [ ClassName "nav-link active" ] [ str "My Articles" ] ]

                li [ ClassName "nav-item" ] [ a [ ClassName "nav-link" ] [ str "Favorited Articles" ] ] ] ]


let view dispatch model =
    div [ ClassName "profile-page" ]
        [ (match model.Profile with
           | Success profile -> userInfo profile

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
                                [ articlesToggle

                                  (match model.ArticlesView with
                                   | AuthorArticles(Success articles)
                                   | FavoritedArticles(Success articles) ->
                                       fragment [] (List.map article articles.Articles)

                                   | _ -> empty) ] ] ] ] ]
