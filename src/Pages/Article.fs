module Pages.Article

open Elmish
open Fable.React
open Fable.React.Props
open Fable.RemoteData

open Types.Article
open Router
open Types
open Api

// TYPES

type Model =
    { Article: RemoteData<string list, Article>
      Comments: RemoteData<string list, Comment list>
      Session: Session option }

type Msg =
    | ArticleFetched of RemoteData<string list, Article>
    | CommentsFetched of RemoteData<string list, Comment list>


// COMMANDS

let private fetchArticle slug = Cmd.OfAsync.perform Articles.fetchArticle slug ArticleFetched

let private fetchComments slug = Cmd.OfAsync.perform Articles.fetchComments slug CommentsFetched


// STATE

let init session slug =
    { Article = Loading
      Comments = Loading
      Session = session },
    Cmd.batch
        [ fetchArticle slug
          fetchComments slug ]

let update msg model =
    match msg with
    | ArticleFetched data -> { model with Article = data }, Cmd.none

    | CommentsFetched data -> { model with Comments = data }, Cmd.none


// VIEW

let private comment (comment: Comment) =
    div [ ClassName "card" ]
        [ div [ ClassName "card-block" ] [ p [ ClassName "card-text" ] [ str comment.Body ] ]

          div [ ClassName "card-footer" ]
              [ a [ ClassName "comment-author" ]  // TODO: link to comment author
                    [ img
                        [ ClassName "comment-author-img"
                          Src comment.Author.Image ]

                      str " "

                      a [ ClassName "comment-author" ] [ str comment.Author.Username ] // TODO: link to comment author

                      span [ ClassName "date-posted" ] [ str <| comment.CreatedAt.ToLongDateString() ] ] ] ]

let private commentForm dispatch =
    form [ ClassName "card comment-form" ]
        [ div [ ClassName "card-block" ]
              [ textarea
                  [ ClassName "form-control"
                    Placeholder "Write a comment..."
                    Rows 3 ] [] ]
          div [ ClassName "card-footer" ]
              [ img
                  [ Src "http://i.imgur.com/Qr71crq.jpg"
                    ClassName "comment-author-img" ]
                button [ ClassName "btn btn-sn btn-primary" ] [ str "Post Comment" ] ] ] // TODO: add new comment

let private comments dispatch session comments =
    div [ ClassName "row" ]
        [ div [ ClassName "cols-xs-12 col-md-8 offset-md-2" ]
              [ (match session with
                 | Some _ -> commentForm dispatch

                 | None ->
                     p []
                         [ a [ href Login ] [ str "Sign in" ]

                           str " or "

                           a [ href Register ] [ str "sign up" ]

                           str " to add a comment on this article." ])

                (match comments with
                 | Success comments -> fragment [] (List.map comment comments)

                 | _ -> empty) ] ]

let private articleOwnerButtons dispatch article =
    fragment []
        [ a
            [ href <| SessionRoute(EditArticle article.Slug)
              ClassName "btn btn-outline-secondary btn-sm" ]
              [ i [ ClassName "ion-edit" ] []

                str " Edit Article" ]

          str " "

          button [ ClassName "btn btn-outline-danger btn-sm" ]
              [ i [ ClassName "ion-trash-a" ] []

                str " Delete Article" ] ] // TODO: delete an article

let private infoButtons dispatch (session: Session option) (article: Article.Article) =
    match session with
    | Some s when s.Username = article.Author.Username -> articleOwnerButtons dispatch article

    | _ ->
        fragment []
            [ button [ ClassName "btn btn-sm btn-outline-secondary" ]
                  [ i [ ClassName "ion-plus-round" ] []

                    str <| sprintf " Follow %s" article.Author.Username

                    span [ ClassName "counter" ] [ str <| sprintf "(10)" ] ] // TODO: author followers

              str "  "

              button [ ClassName "btn btn-sm btn-outline-primary" ]
                  [ i [ ClassName "ion-heart" ] []

                    str " Favorite Post "

                    span [ ClassName "counter" ] [ str <| sprintf "(%i)" article.FavoritesCount ] ] ]


let private banner dispatch session article =
    div [ ClassName "banner" ]
        [ div [ ClassName "container" ]
              [ h1 [] [ str article.Title ]

                div [ ClassName "article-meta" ]
                    [ a [] [ img [ Src article.Author.Image ] ] // TODO: link to author profile

                      div [ ClassName "info" ]
                          [ a [ ClassName "author" ] [ str article.Author.Username ] // TODO: link to author profile

                            span [ ClassName "date" ] [ str <| article.CreatedAt.ToLongDateString() ] ]

                      infoButtons dispatch session article ] ] ]

let view dispatch (model: Model) =
    div [ ClassName "article-page" ]
        [ (match model.Article with
           | Success article ->
               fragment []
                   [ banner dispatch model.Session article
                     div [ ClassName "container page" ]
                         [ div [ ClassName "row article-content" ]
                               [ div [ ClassName "col-md-12" ] [ str article.Body ] ]

                           hr []

                           div [ ClassName "article-actions" ]
                               [ div [ ClassName "article-meta" ]
                                     [ a [] [ img [ Src article.Author.Image ] ] // TODO: link to author

                                       div [ ClassName "info" ]
                                           [ a [ ClassName "author" ] [ str article.Author.Username ] // TODO: link to author

                                             span [ ClassName "date" ] [ str <| article.CreatedAt.ToLongDateString() ] ]

                                       button [ ClassName "btn tbn-sm btn-outline-secondary" ]
                                           [ i [ ClassName "ion-plus-round" ] []

                                             str <| sprintf " Follow %s " article.Author.Username

                                             span [ ClassName "counter" ] [ str <| sprintf "(10)" ] ] ] ] // TODO: following counter

                           comments dispatch model.Session model.Comments ] ]

           | _ -> empty) ]
