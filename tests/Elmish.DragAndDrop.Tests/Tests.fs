namespace Elmish.DragAndDrop.Tests

open System
open Elmish
open Expecto

module ListUpdateTests =
    open Elmish.DragAndDrop

    let li = [ "item 1" ; "item 2"; "item 3"; "item 4" ]

    let rotate dragIndex dropIndex li =
        DragAndDrop.listUpdate dragIndex dropIndex li

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

module ListRemoveTests =
    open Elmish.DragAndDrop

    let li = [ "item 1"; "item 2"; "item 3"; "item 4" ]

    let remove dragIndex li =
        DragAndDrop.listRemove dragIndex li

    [<Tests>]
    let tests =
        testList "List Remove tests" [
            testCase "Removing [0] item removes that item" <| fun _ ->
                let output = remove 0 li
                let expected = [ "item 2"; "item 3"; "item 4" ]
                Expect.equal output expected "Did not remove expected item."
            testCase "Remove [1] item removes that item" <| fun _ ->
                let output = remove 1 li
                let expected = [ "item 1"; "item 3"; "item 4" ]
                Expect.equal output expected "Did not remove expected item."
            testCase "Remove [2] item removes that item" <| fun _ ->
                let output = remove 2 li
                let expected = [ "item 1"; "item 2"; "item 4" ]
                Expect.equal output expected "Did not remove expected item."
            testCase "Removing [3] item removes that item" <| fun _ ->
                let output = remove 3 li
                let expected = [ "item 1"; "item 2"; "item 3" ]
                Expect.equal output expected "Did not remove expected item."
        ]

module ExternalHelpersTests =
    open Elmish.DragAndDrop

    let model = {
            StartIndex = 1
            DragIndex = 1
            DropIndex = 2
            DragCounter = 0
            StartPosition = { X = 1.; Y = 1. }
            CurrentPosition = { X = 2.; Y = 2.}
            DragElementId = "drag-element"
            DropElementId = "drop-element"
            DragElement = None
            DropElement = None
            StartBucket = None
            DropBucket = None
        }

    let li = [ "item 1"; "item 2"; "item 3"; "item 4" ]

    [<Tests>]
    let tests =
        testList "External helper function tests" [
            testCase "tryGetDraggedItem returns none with no bucket and current drag" <| fun _ ->
                let input = None
                let output = tryGetDraggedItem input None li
                let expected = None
                Expect.equal output expected "Should not get a dragged item with no current drag"
            testCase "tryGetDraggedItem returns none with bucket and no current drag" <| fun _ ->
                let input = None
                let output = tryGetDraggedItem input (Some 1) li
                let expected = None
                Expect.equal output expected "Should not get a dragged item with no current drag"
            testCase "tryGetDraggedItem returns element during drag with no bucket" <| fun _ ->
                let output = tryGetDraggedItem (Some model) None li
                let expected = Some "item 2"
                Expect.equal output expected "Did not find expected item at drag index"
            testCase "tryGetDraggedItem returns element during drag with a bucket" <| fun _ ->
                let model = { model with StartBucket = Some 1}
                let output = tryGetDraggedItem (Some model) (Some 1) li
                let expected = Some "item 2"
                Expect.equal output expected "Did not find expected item at drag index"
            testCase "tryGetDraggedItem returns none if the input list is smaller than the given index" <| fun _ ->
                let input = Some model
                let output = tryGetDraggedItem input None [ "hello world" ]
                let expected = None
                Expect.equal output expected "Should not get an item with an input list too small"

            testCase "isDraggedItemByIndex returns false when model is none" <| fun _ ->
                let input = None
                let output = isDraggedItemByIndex input 0 (Some 0)
                let expected = false
                Expect.equal output expected "Should not find a drag item index with no dragging"
            testCase "isDraggedItemByIndex returns false when dragged item has a bucket but input does not" <| fun _ ->
                let input = Some {model with StartBucket = Some 0}
                let output = isDraggedItemByIndex input 0 None
                Expect.isFalse output "Should not find drag index when buckets do not match"
            testCase "isDraggedItemByIndex returns false when dragged item is not in a bucket but input is" <| fun _ ->
                let input = Some model
                let output = isDraggedItemByIndex input 0 (Some 1)
                Expect.isFalse output "Should not find drag index when buckets do not match"
            testCase "isDraggedItemByIndex returns false when indexes do not match" <| fun _ ->
                let input = Some {model with StartBucket = Some 0}
                let output = isDraggedItemByIndex input 0 (Some 0)
                Expect.isFalse output "Should not find drag index when indexes do not match"
            testCase "isDraggedItemByIndex returns true when indexes and buckets match" <| fun _ ->
                let input = Some {model with StartBucket = Some 0}
                let output = isDraggedItemByIndex input 1 (Some 0)
                Expect.isTrue output "Shoud find index when all inputs match"
        ]