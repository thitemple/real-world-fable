module Pages.Editor

open Elmish
open Fable.React
open Fable.React.Props
open Fable.RemoteData

open Types.Article
open Types
open Api


// TYPES

type Mode =
    | Editing of string
    | Creating

type Model =
    { Session: Session
      TagPlaceholder: string
      Mode: Mode
      Errors: string list
      Article: RemoteData<string list, SimplifiedArticle> }

type Msg =
    | SetTitle of string
    | SetDescription of string
    | SetBody of string
    | SetTag of string
    | AddTag of string
    | RemoveTag of string
    | SaveArticle
    | ArticleSaved of RemoteData<string list, Article.Article>


// COMMANDS

let createArticle session article = Cmd.OfAsync.perform (Articles.createArticle session) article ArticleSaved

let saveArticle session slug article =
    Cmd.OfAsync.perform (Articles.updateArticle session) (slug, article) ArticleSaved


// STATE

let initNew session =
    { Session = session
      TagPlaceholder = ""
      Mode = Creating
      Errors = []
      Article =
          Success
              { Title = ""
                Description = ""
                Body = ""
                TagList = Set.empty } }, Cmd.none

let private updateArticle transform model =
    match model.Article with
    | Success article -> { model with Article = Success <| transform article }, Cmd.none

    | _ -> model, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | SetTitle title -> updateArticle (fun a -> { a with Title = title }) model

    | SetDescription description -> updateArticle (fun a -> { a with Description = description }) model

    | SetBody body -> updateArticle (fun a -> { a with Body = body }) model

    | SetTag tag -> { model with TagPlaceholder = tag }, Cmd.none

    | AddTag tag ->
        let (model, cmd) = updateArticle (fun a -> { a with TagList = Set.add tag a.TagList }) model
        { model with TagPlaceholder = "" }, cmd

    | RemoveTag tag -> updateArticle (fun a -> { a with TagList = Set.remove tag a.TagList }) model

    | SaveArticle ->
        match model.Article with
        | Success article ->
            let result = validateArticle article

            match result, model.Mode with
            | Ok validatedArticle, Creating -> model, createArticle model.Session validatedArticle

            | Ok validatedArticle, Editing slug -> model, saveArticle model.Session slug validatedArticle

            | Error err, _ -> { model with Errors = [ err ] }, Cmd.none

        | _ -> model, Cmd.none

    | ArticleSaved(Success article) -> model, Router.newUrl <| Router.Route.Article article.Slug

    | ArticleSaved(Failure errs) -> { model with Errors = errs }, Cmd.none

    | ArticleSaved _ -> model, Cmd.none



// VIEW

let private tagList dispatch tags =
    let tag t =
        span [ ClassName "tag-default tag-pill" ]
            [ i
                [ ClassName "ion-close-round"
                  OnClick(fun _ -> dispatch <| RemoveTag t) ] []
              str t ]
    div [ ClassName "tag-list" ]
        (tags
         |> Set.toList
         |> List.map tag)

let private form dispatch tag (article: SimplifiedArticle) =
    form [ OnSubmit(fun _ -> dispatch SaveArticle) ]
        [ fieldset []
              [ fieldset [ ClassName "form-group" ]
                    [ input
                        [ Type "text"
                          ClassName "form-control form-control-lg"
                          Value article.Title
                          OnChange(fun ev -> dispatch <| SetTitle ev.Value)
                          Placeholder "Article Title" ] ]

                fieldset [ ClassName "form-group" ]
                    [ input
                        [ Type "text"
                          ClassName "form-control"
                          Value article.Description
                          OnChange(fun ev -> dispatch <| SetDescription ev.Value)
                          Placeholder "What's this article about?" ] ]

                fieldset [ ClassName "form-group" ]
                    [ textarea
                        [ ClassName "form-control"
                          Rows 8
                          Value article.Body
                          OnChange(fun ev -> dispatch <| SetBody ev.Value)
                          Placeholder "Write your article (in markdown)" ] [] ]

                fieldset [ ClassName "form-group" ]
                    [ input
                        [ Type "text"
                          ClassName "form-control"
                          Value tag
                          OnKeyDown(fun ev ->
                              if ev.key = "Enter" then
                                  ev.stopPropagation()
                                  dispatch <| AddTag ev.Value)
                          OnChange(fun ev -> dispatch <| SetTag ev.Value)
                          Placeholder "Enter tags" ]

                      tagList dispatch article.TagList ]

                button [ ClassName "btn btn-lg pull-xs-right btn-primary" ] [ str "Publish Article" ] ] ]

let view dispatch (model: Model) =
    div [ ClassName "editor-page" ]
        [ div [ ClassName "container page" ]
              [ div [ ClassName "row" ]
                    [ div [ ClassName "col-md-10 offset-md-1 col-xs-12" ]
                          [ (match model.Article with
                             | Success article -> form dispatch model.TagPlaceholder article

                             | _ -> empty) ] ] ] ]
