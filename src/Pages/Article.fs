module Pages.Article

open Elmish
open Fable.React
open Fable.React.Props
open Fable.RemoteData

open Shared.Types
open Shared.Api

type Model =
    { Article: RemoteData<string list, Article>
      Comments: RemoteData<string list, Comment list> }

type Msg =
    | ArticleFetched of RemoteData<string list, Article>
    | CommentsFetched of RemoteData<string list, Comment list>

let private fetchArticle slug = Cmd.OfAsync.perform Articles.fetchArticle slug ArticleFetched

let private fetchComments slug = Cmd.OfAsync.perform Articles.fetchComments slug CommentsFetched

let init slug =
    { Article = Loading
      Comments = Loading },
    Cmd.batch
        [ fetchArticle slug
          fetchComments slug ]

let update msg model: Model * Cmd<Msg> =
    match msg with
    | ArticleFetched data -> { model with Article = data }, Cmd.none

    | CommentsFetched data -> { model with Comments = data }, Cmd.none

let comments data =
    match data with
    | Success comments ->
        fragment []
            (List.map
                (fun comment ->
                div [ ClassName "card" ]
                    [ div [ ClassName "card-block" ] [ p [ ClassName "card-text" ] [ str comment.Body ] ]
                      div [ ClassName "card-footer" ]
                          [ a [ ClassName "comment-author" ]
                                [ img
                                    [ ClassName "comment-author-img"
                                      Src comment.Author.Image ]
                                  str " "
                                  a [ ClassName "comment-author" ] [ str comment.Author.Username ]
                                  span [ ClassName "date-posted" ] [ str <| comment.CreatedAt.ToLongDateString() ] ] ] ])
                 comments)
    | _ -> fragment [] []

let view dispatch (model: Model) =
    div [ ClassName "article-page" ]
        [ (match model.Article with
           | Success article ->
               fragment []
                   [ div [ ClassName "banner" ]
                         [ div [ ClassName "container" ]
                               [ h1 [] [ str article.Title ]
                                 div [ ClassName "article-meta" ]
                                     [ a [] [ img [ Src article.Author.Image ] ]
                                       div [ ClassName "info" ]
                                           [ a [ ClassName "author" ] [ str article.Author.Username ]
                                             span [ ClassName "date" ] [ str <| article.CreatedAt.ToLongDateString() ] ]
                                       button [ ClassName "btn btn-sm btn-outline-secondary" ]
                                           [ i [ ClassName "ion-plus-round" ] []
                                             str <| sprintf " Follow %s" article.Author.Username
                                             span [ ClassName "counter" ] [ str <| sprintf "(10)" ] ] // TODO: author followers
                                       str "  "
                                       button [ ClassName "btn btn-sm btn-outline-primary" ]
                                           [ i [ ClassName "ion-heart" ] []
                                             str " Favorite Post "
                                             span [ ClassName "counter" ]
                                                 [ str <| sprintf "(%i)" article.FavoritesCount ] ] ] ] ]
                     div [ ClassName "container page" ]
                         [ div [ ClassName "row article-content" ]
                               [ div [ ClassName "col-md-12" ] [ str article.Body ] ]
                           hr []
                           div [ ClassName "article-actions" ]
                               [ div [ ClassName "article-meta" ]
                                     [ a [] [ img [ Src article.Author.Image ] ]
                                       div [ ClassName "info" ]
                                           [ a [ ClassName "author" ] [ str article.Author.Username ]
                                             span [ ClassName "date" ] [ str <| article.CreatedAt.ToLongDateString() ] ]
                                       button [ ClassName "btn tbn-sm btn-outline-secondary" ]
                                           [ i [ ClassName "ion-plus-round" ] []
                                             str <| sprintf " Follow %s " article.Author.Username
                                             span [ ClassName "counter" ] [ str <| sprintf "(10)" ] ] ] ]
                           div [ ClassName "row" ]
                               [ div [ ClassName "cols-xs-12 col-md-8 offset-md-2" ]
                                     [ form [ ClassName "card comment-form" ]
                                           [ div [ ClassName "card-block" ]
                                                 [ textarea
                                                     [ ClassName "form-control"
                                                       Placeholder "Write a comment..."
                                                       Rows 3 ] [] ]
                                             div [ ClassName "card-footer" ]
                                                 [ img
                                                     [ Src "http://i.imgur.com/Qr71crq.jpg"
                                                       ClassName "comment-author-img" ]
                                                   button [ ClassName "btn btn-sn btn-primary" ] [ str "Post Comment" ] ] ]

                                       comments model.Comments ] ] ] ]

           | _ -> str "") ]
