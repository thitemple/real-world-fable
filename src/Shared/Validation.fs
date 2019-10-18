module Validation

open System

let isEmpty errorMsg get value =
    if String.IsNullOrWhiteSpace <| get value then Error errorMsg
    else Ok value
