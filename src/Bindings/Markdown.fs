module Fable.Markdown

open Fable.Core

type IMarkdown =
    abstract toHTML: string -> string

[<Import("markdown", "markdown")>]
let markdown: IMarkdown = jsNative
 