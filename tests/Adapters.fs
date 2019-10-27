namespace Tests

open Fable.Core

[<AutoOpen>]
module MochaBindings =

    [<Global>]
    let describe (name: string) (f: unit -> unit) = jsNative

    [<Global>]
    let it (msg: string) (f: unit -> unit) = jsNative


[<AutoOpen>]
module Matchers =

    let inline equal (expected: 'T) (actual: 'T): unit = Testing.Assert.AreEqual(expected, actual)
