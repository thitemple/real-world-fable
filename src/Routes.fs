module Routes

open Elmish.UrlParser

type Page =
    | Articles

let pageParser : Parser<Page -> Page, Page> = 
    oneOf [
        map Articles top
    ]

let toHash page =
    match page with
    | Articles -> "#/"