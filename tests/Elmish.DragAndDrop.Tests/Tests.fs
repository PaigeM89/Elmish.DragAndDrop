namespace Elmish.DragAndDrop.Tests

open System
open Elmish
open Expecto
open Elmish.DragAndDrop

module ListUpdateTests =

    let li = [ "item 1" ; "item 2"; "item 3"; "item 4" ]

    let rotate dragIndex dropIndex li =
        DragAndDrop.listUpdate Rotate dragIndex dropIndex li

    [<Tests>]
    let tests =
        testList "Update list tests"[
            testList "Rotate tests" [
                testCase "Single item list returns that item" <| fun _ ->
                    let output = rotate 0 0 [ "item 1" ]
                    let expected = [ "item 1" ]
                    Expect.equal output expected "Altered a single item list"
                testCase "List with equal indexes is unaltered" <| fun _ ->
                    let output = rotate 2 2 li
                    let expected = li
                    Expect.equal output expected "Equal indexes should return an unaltered list"
                testCase "drag index lower than drop index shifts correctly" <| fun _ ->
                    let output = rotate 0 3 li
                    let expected = [ "item 2"; "item 3"; "item 4"; "item 1" ]
                    Expect.equal output expected "Should have shifted list elements as expected."
                testCase "drag index higher than drop index shifts correctly" <| fun _ ->
                    let output = rotate 2 0 li
                    let expected = [ "item 3"; "item 1"; "item 2"; "item 4" ]
                    Expect.equal output expected "Should have shifted list elements as expected."
                testCase "moving last item shifts other items into last" <| fun _ ->
                    let output = rotate 3 1 li
                    let expected = [ "item 1"; "item 4"; "item 2"; "item 3" ]
                    Expect.equal output expected "Should have shifted list elements as expected."
                testCase "moving first item shifts other items into first" <| fun  _ ->
                    let output = rotate 0 2 li
                    let expected = [ "item 2"; "item 3"; "item 1"; "item 4" ]
                    Expect.equal output expected "Should have shifted list elements as expected."
                testCase "list of 2 items allows items to swap" <| fun _ ->
                    let output = rotate 0 1 [ "item 1"; "item 2" ]
                    let expected = [ "item 2"; "item 1" ]
                    Expect.equal output expected "List of 2 items should allow swapping."
            ]
        ]
