module Tests.ArticleTests

open Fable.RemoteData

open Types
open Pages.Article

describe "Article Page" <| fun _ ->

    describe "init" <| fun _ ->

        it "initializes the model unauthenticated" <| fun () ->
            init None "the-slug"
            |> fst
            |> equal
                { Article = Loading
                  Comments = Loading
                  NewComment = ""
                  Errors = []
                  Authentication = Unauthenticated }

        it "initializes the model authenticated" <| fun () ->
            let session =
                { Token = ""
                  Username = "" }
            init (Some session) "the slug"
            |> fst
            |> equal
                { Article = Loading
                  Comments = Loading
                  NewComment = ""
                  Errors = []
                  Authentication =
                      Authenticated
                          { Session = session
                            User = Loading } }
