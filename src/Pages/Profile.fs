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
    { Profile: RemoteData<string list, Profile>
      Username: string
      ArticlesView: ArticlesView
      Articles: RemoteData<string list, ArticlesList>
      Session: Session }

type Msg =
    | ProfileLoaded of RemoteData<string list, Profile>
    | ArticlesLoaded of RemoteData<string list, ArticlesList>
    | ToggleFavoriteArticle of FullArticle
    | FavoriteArticleToggled of RemoteData<string list, FullArticle>


// COMMANDS

let private fetchProfile username = Cmd.OfAsync.perform Profiles.fetchProfile username ProfileLoaded


let private fetchArticlesFromAuthor username =
    Cmd.OfAsync.perform Articles.fetchArticlesFromAuthor username ArticlesLoaded


let private favArticle session article =
    Cmd.OfAsync.perform Articles.favoriteArticle
        {| Session = session
           Article = article |} FavoriteArticleToggled


let private unfavArticle session article =
    Cmd.OfAsync.perform Articles.unfavoriteArticle
        {| Session = session
           Article = article |} FavoriteArticleToggled


// STATE

let init session username =
    { Profile = Loading
      Username = username
      ArticlesView = AuthorArticles
      Articles = Loading
      Session = session },
    Cmd.batch
        [ fetchProfile username
          fetchArticlesFromAuthor username ]


let update msg model =
    match msg with
    | ProfileLoaded data -> { model with Profile = data }, Cmd.none

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


let private article dispatch (article: FullArticle) =
    div [ ClassName "article-preview" ]
        [ div [ ClassName "article-meta" ]
              [ a [ href <| SessionRoute(Profile article.Author.Username) ] [ img [ Src article.Author.Image ] ]

                div [ ClassName "info" ]
                    [ a [ href <| SessionRoute(Profile article.Author.Username) ] [ str article.Author.Username ]

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


let private articlesToggle =
    div [ ClassName "articles-toggle" ]
        [ ul [ ClassName "nav nav-pills outline-active" ]  // TODO: toggle between author's articles and favorites
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

                                  (match model.Articles with
                                   | Success articles -> fragment [] (List.map (article dispatch) articles.Articles)

                                   | _ -> empty) ] ] ] ] ]
