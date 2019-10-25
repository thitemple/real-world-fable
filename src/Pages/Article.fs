module Pages.Article

open Elmish
open Fable.React
open Fable.React.Props
open Fable.RemoteData
open System

open Types.Article
open Router
open Types
open Api
open Elements


// TYPES

type AuthenticatedSession =
    { Session: Session
      User: RemoteData<string list, User> }

type Authentication =
    | Authenticated of AuthenticatedSession
    | Unauthenticated

type Model =
    { Article: RemoteData<string list, FullArticle>
      Comments: RemoteData<string list, Comment list>
      NewComment: string
      Errors: string list
      Authentication: Authentication }

type Msg =
    | ArticleFetched of RemoteData<string list, FullArticle>
    | CommentsFetched of RemoteData<string list, Comment list>
    | CommentCreated of RemoteData<string list, Comment>
    | UserFetched of RemoteData<string list, User>
    | DeleteArticle of FullArticle
    | ArticleDeleted of RemoteData<string list, unit>
    | ToggleFollowAuthor of Author
    | FollowAuthorToggled of RemoteData<string list, Author>
    | SetNewComment of string
    | SubmitComment


// COMMANDS

let private fetchArticle session slug =
    match session with
    | Some s ->
        Cmd.OfAsync.perform Articles.fetchArticleWithSession
            {| Session = s
               Slug = slug |} ArticleFetched

    | None -> Cmd.OfAsync.perform Articles.fetchArticle slug ArticleFetched


let private fetchComments slug = Cmd.OfAsync.perform Articles.fetchComments slug CommentsFetched


let private createComment session slug comment =
    let payload =
        {| Session = session
           Slug = slug
           CommentBody = comment |}
    Cmd.OfAsync.perform Articles.createComment payload CommentCreated


let private fetchUser session = Cmd.OfAsync.perform Users.fetchUser session UserFetched

let private deleteArticle session slug =
    Cmd.OfAsync.perform Articles.deleteArticle
        {| Session = session
           Slug = slug |} ArticleDeleted

let private followAuthor session author =
    Cmd.OfAsync.perform Profiles.createFollower
        {| Session = session
           Author = author |} FollowAuthorToggled

let private unfollowAuthor session author =
    Cmd.OfAsync.perform Profiles.deleteFollower
        {| Session = session
           Author = author |} FollowAuthorToggled

// STATE

let init session slug =
    let (authentication, cmdUser) =
        match session with
        | Some s ->
            (Authenticated
                { Session = s
                  User = Loading }), fetchUser s

        | None -> Unauthenticated, Cmd.none

    { Article = Loading
      Comments = Loading
      NewComment = ""
      Errors = []
      Authentication = authentication },
    Cmd.batch
        [ fetchArticle session slug
          fetchComments slug
          cmdUser ]


let update msg model =
    match msg with
    | ArticleFetched data -> { model with Article = data }, Cmd.none

    | CommentsFetched data -> { model with Comments = data }, Cmd.none

    | SetNewComment comment -> { model with NewComment = comment }, Cmd.none

    | SubmitComment ->
        let errors =
            if String.IsNullOrWhiteSpace model.NewComment then [ "comment can't be blank" ]
            else []

        match List.isEmpty errors, model.Authentication, model.Article with
        | true, Authenticated { Session = session }, Success(article) ->
            model, createComment session article.Slug model.NewComment

        | false, _, _ -> { model with Errors = errors }, Cmd.none

        | _ -> model, Cmd.none

    | CommentCreated(Success comment) ->
        model.Comments
        |> map (fun comments ->
            { model with
                  Comments = Success <| comment :: comments
                  NewComment = ""
                  Errors = [] }, Cmd.none)
        |> withDefault (model, Cmd.none)

    | CommentCreated(Failure errors) -> { model with Errors = errors }, Cmd.none

    | CommentCreated _ -> model, Cmd.none

    | UserFetched data ->
        match model.Authentication with
        | Authenticated auth ->
            let auth = { auth with User = data }
            { model with Authentication = Authenticated auth }, Cmd.none

        | _ -> model, Cmd.none

    | DeleteArticle article ->
        match model.Authentication with
        | Authenticated auth -> model, deleteArticle auth.Session article.Slug

        | _ -> model, Cmd.none

    | ArticleDeleted(Success _) -> model, newUrl Articles

    | ArticleDeleted(Failure errors) -> { model with Errors = errors }, Cmd.none

    | ArticleDeleted _ -> model, Cmd.none

    | ToggleFollowAuthor author ->
        match model.Authentication, author.Following with
        | Authenticated auth, false -> model, followAuthor auth.Session author

        | Authenticated auth, true -> model, unfollowAuthor auth.Session author

        | _ -> model, Cmd.none

    | FollowAuthorToggled(Success author) ->
        let article = map (fun (article: FullArticle) -> { article with Author = author }) model.Article
        { model with Article = article }, Cmd.none

    | FollowAuthorToggled _ -> failwith "Not Implemented"


// VIEW

let private comment (comment: Comment) =
    div [ ClassName "card" ]
        [ div [ ClassName "card-block" ] [ p [ ClassName "card-text" ] [ str comment.Body ] ]

          div [ ClassName "card-footer" ]
              [ a
                  [ ClassName "comment-author"
                    href <| Profile comment.Author.Username ]
                    [ img
                        [ ClassName "comment-author-img"
                          Src comment.Author.Image ]

                      str " "

                      str comment.Author.Username

                      span [ ClassName "date-posted" ] [ str <| comment.CreatedAt.ToLongDateString() ] ] ] ]


let private commentForm dispatch (user: User) errors newComment =
    form
        [ ClassName "card comment-form"
          OnSubmit(fun _ -> dispatch SubmitComment) ]
        [ errorsList errors
          div [ ClassName "card-block" ]
              [ textarea
                  [ ClassName "form-control"
                    Placeholder "Write a comment..."
                    Value newComment
                    OnChange(fun ev -> dispatch <| SetNewComment ev.Value)
                    Rows 3 ] [] ]
          div [ ClassName "card-footer" ]
              [ img
                  [ Src
                    <| Option.defaultWith (fun _ -> "https://static.productionready.io/images/smiley-cyrus.jpg")
                           user.Image
                    ClassName "comment-author-img" ]
                button [ ClassName "btn btn-sn btn-primary" ] [ str "Post Comment" ] ] ]


let private comments dispatch model =
    div [ ClassName "row" ]
        [ div [ ClassName "cols-xs-12 col-md-8 offset-md-2" ]
              [ (match model.Authentication with
                 | Authenticated { User = (Success user) } -> commentForm dispatch user model.Errors model.NewComment

                 | _ ->
                     p []
                         [ a [ href Login ] [ str "Sign in" ]

                           str " or "

                           a [ href Register ] [ str "sign up" ]

                           str " to add a comment on this article." ])

                (match model.Comments with
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

          button
              [ ClassName "btn btn-outline-danger btn-sm"
                OnClick(fun _ -> dispatch <| DeleteArticle article) ]
              [ i [ ClassName "ion-trash-a" ] []

                str " Delete Article" ] ]


let private infoButtons dispatch authentication (article: FullArticle) =
    match authentication with
    | Authenticated auth when auth.Session.Username = article.Author.Username -> articleOwnerButtons dispatch article

    | Authenticated _ ->
        fragment []
            [ button
                [ classList
                    [ ("btn", true)
                      ("btn-sm", true)
                      ("btn-outline-secondary", not article.Author.Following)
                      ("btn-secondary", article.Author.Following) ]
                  OnClick(fun _ -> dispatch <| ToggleFollowAuthor article.Author) ]
                  [ i [ ClassName "ion-plus-round" ] []

                    str <| sprintf " %s %s"
                               (if article.Author.Following then "Unfollow"
                                else "Follow") article.Author.Username ]

              str "  "

              button [ ClassName "btn btn-sm btn-outline-primary" ]  // TODO: favorite an article
                  [ i [ ClassName "ion-heart" ] []

                    str " Favorite Post "

                    span [ ClassName "counter" ] [ str <| sprintf "(%i)" article.FavoritesCount ] ] ]

    | _ -> empty


let private banner dispatch authentication article =
    div [ ClassName "banner" ]
        [ div [ ClassName "container" ]
              [ h1 [] [ str article.Title ]

                div [ ClassName "article-meta" ]
                    [ a [ href <| Profile article.Author.Username ] [ img [ Src article.Author.Image ] ]

                      div [ ClassName "info" ]
                          [ a
                              [ ClassName "author"
                                href <| Profile article.Author.Username ] [ str article.Author.Username ]

                            span [ ClassName "date" ] [ str <| article.CreatedAt.ToLongDateString() ] ]

                      infoButtons dispatch authentication article ] ] ]


let view dispatch (model: Model) =
    div [ ClassName "article-page" ]
        [ (match model.Article with
           | Success article ->
               fragment []
                   [ banner dispatch model.Authentication article
                     div [ ClassName "container page" ]
                         [ div [ ClassName "row article-content" ]
                               [ div [ ClassName "col-md-12" ] [ str article.Body ] ]

                           hr []

                           div [ ClassName "article-actions" ]
                               [ div [ ClassName "article-meta" ]
                                     [ a [ href <| Profile article.Author.Username ]
                                           [ img [ Src article.Author.Image ] ]

                                       div [ ClassName "info" ]
                                           [ a
                                               [ ClassName "author"
                                                 href <| Profile article.Author.Username ]
                                                 [ str article.Author.Username ]

                                             span [ ClassName "date" ] [ str <| article.CreatedAt.ToLongDateString() ] ]

                                       button [ ClassName "btn tbn-sm btn-outline-secondary" ]
                                           [ i [ ClassName "ion-plus-round" ] []

                                             str <| sprintf " Follow %s " article.Author.Username

                                             span [ ClassName "counter" ] [ str <| sprintf "(10)" ] ] ] ] // TODO: following counter

                           comments dispatch model ] ]

           | _ -> empty) ]
