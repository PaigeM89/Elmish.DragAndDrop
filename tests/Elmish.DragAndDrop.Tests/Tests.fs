namespace Elmish.DragAndDrop.Tests

open System
open Elmish
open Expecto

module MoveItemTests =
    open Elmish.DragAndDrop3

    let singleList = [[ "item 0" ; "item 1"; "item 2"; "item 3"; "item 4" ]]

    let modelFromList li = Model.createWithItems li
    let modelFromMultiList li = Model.createWithItemsMultiList li

    let model = Model.createWithItemsMultiList singleList

    [<Tests>]
    let tests = 
        testList "Drag and drop 3 move item tests" [
            testCase "Moving an item to the same spot does not change the list" <| fun _ ->
                let startingList = model.Items
                let loc = (0, 0, "item 0")
                let mdl, cmd = DragAndDrop3.moveItem loc "item 0" model
                Expect.equal mdl.Items startingList "Should not alter the input"
            testCase "Moving an item to the same spot at the end of the list does not change the list" <| fun _ ->
                let startingList = model.Items
                let loc = (0, 4, "item 4")
                let mdl, cmd = DragAndDrop3.moveItem loc "item 4" model
                Expect.equal mdl.Items startingList "Should not alter the input"
            testCase "Swapping the first two items swaps those items" <| fun _ ->
                let expected = [ "item 1" ; "item 0"; "item 2"; "item 3"; "item 4" ] |> modelFromList
                let loc = (0, 1, "item 1")
                let mdl, cmd = DragAndDrop3.moveItem loc "item 0" model
                Expect.equal mdl.Items expected.Items "Should not alter the input"
        ]

module ListTests =
    open Elmish.List

    [<Tests>]
    let tests =
        testList "List module addons tests" [
            testList "tryTake tests" [
                testCase "tryTake returns empty list for negative amount" <| fun _ ->
                    let output = tryTake -1 [1; 2; 3;]
                    Expect.equal output [] "Should get empty list"
                testCase "tryTake returns empty list for 0" <| fun _ ->
                    let output = tryTake 0 [1; 2; 3;]
                    Expect.equal output [] "Should get empty list"
                testCase "tryTake returns full list when amount equals length" <| fun _ ->
                    let output = tryTake 3 [ 1; 2; 3]
                    Expect.equal output [1;2;3] "Should get full list"
                testCase "tryTake returns full list when amount exceeds length" <| fun _ ->
                    let output = tryTake 4 [1;2;3]
                    Expect.equal output [1;2;3] "should get full list"
                testCase "tryTake returns 2 items" <| fun _ ->
                    let output = tryTake 2 [1;2;3]
                    Expect.equal output [1;2;] "Should get partial list"
                testCase "tryTake returns 1 item" <| fun _ ->
                    let output = tryTake 1 [1;2;3]
                    Expect.equal output [1] "Should get partial list"
            ]
            testList "removeAt tests" [
                testCase "removeAt returns original list if index exceeds list length" <| fun _ ->
                    let output = removeAt 4 [1;2;3;]
                    Expect.equal output [1;2;3;] "Should get full list"
                testCase "removeAt returns tail if remove index is 0" <| fun _ ->
                    let output = removeAt 0 [1;2;3]
                    Expect.equal output [2;3;] "Should get list tail"
                testCase "removeAt removes interior element" <| fun _ ->
                    let output = removeAt 1 [1;2;3;4]
                    Expect.equal output [1;3;4] "Should remove inner element"
                testCase "removeAt removes interior element (part 2)" <| fun _ ->
                    let output = removeAt 2 [1;2;3;4]
                    Expect.equal output [1;2; 4] "Should remove inner element"
                testCase "removeAt removes last element" <| fun _ ->
                    let output = removeAt 3 [1;2;3;4]
                    Expect.equal output [1;2;3] "Should remove last element"
            ]
            testList "insertAt tests" [
                testCase "insertAt appends item at end if index exceeds length" <| fun _ ->
                    let output = insertAt 10 4 [1;2;3]
                    Expect.equal output [1;2;3;10] "Should put item at end"
                testCase "insertAt appends item at end if index equals length" <| fun _ ->
                    let output = insertAt 10 3 [1;2;3]
                    Expect.equal output [1;2;3;10] "Should put item at end"
                testCase "insertAt appends at head if index is 0" <| fun _ ->
                    let output = insertAt 10 0 [1;2;3]
                    Expect.equal output [10;1;2;3] "Should put item at head"
                testCase "insertAt inserts item at 1st index" <| fun _ ->
                    let output = insertAt 10 1 [1;2;3;4]
                    Expect.equal output [1;10;2;3;4] "Should put item at first index"
                testCase "insertAt inserts item at 2nd index" <| fun _ ->
                    let output = insertAt 10 2 [1;2;3;4]
                    Expect.equal output [1;2; 10;3;4] "Should put item at 2nd index"
                testCase "insertAt inserts item at 3rd index" <| fun _ ->
                    let output = insertAt 10 3 [1;2;3;4]
                    Expect.equal output [1;2; 3; 10;4] "Should put item at 3rd index"
                testCase "insertAt inserts item at 4th index" <| fun _ ->
                    let output = insertAt 10 4 [1;2;3;4]
                    Expect.equal output [1;2; 3; 4; 10] "Should put item at 4th index"
            ]
            testList "replaceAt tests" [
                testCase "replaceAt appends item if index exceeds length" <| fun _ ->
                    let output = replaceAt 10 4 [1;2;3]
                    Expect.equal output [1;2;3;10] "Should append item at end"
                testCase "replaceAt replaces head if index is 0" <| fun _ ->
                    let output = replaceAt 10 0 [1;2;3]
                    Expect.equal output [10;2;3;] "Should replace item at head"
                testCase "replaceAt appends last item if index equals length" <| fun _ ->
                    let output = replaceAt 10 3 [1;2;3]
                    Expect.equal output [1;2;3;10] "Should append item at end"
                testCase "replaceAt replaces item at 1st index" <| fun _ ->
                    let output = replaceAt 10 1 [1;2;3;4]
                    Expect.equal output [1;10;3;4] "Should replace at first index"
                testCase "replaceAt replaces item at 2nd index" <| fun _ ->
                    let output = replaceAt 10 2 [1;2;3;4]
                    Expect.equal output [1;2;10;4] "Should replace at first index"
                testCase "replaceAt replaces item at 3rd index" <| fun _ ->
                    let output = replaceAt 10 3 [1;2;3;4]
                    Expect.equal output [1;2;3;10] "Should replace at first index"
            ]
        ]

module MoveItem2Tests =
    let singleList = [ "item 0" ; "item 1"; "item 2"; "item 3"; "item 4" ]

    let singleSwap si ei li = DragAndDrop3.moveItem2 (0, si) (0, ei) li

    let testBuilder() =
        [
            0, 1, [ "item 1" ; "item 0"; "item 2"; "item 3"; "item 4" ]
            1, 2, [ "item 0" ; "item 2"; "item 1"; "item 3"; "item 4" ]
            2, 3, [ "item 0" ; "item 1"; "item 3"; "item 2"; "item 4" ]
            3, 4, [ "item 0" ; "item 1"; "item 2"; "item 4"; "item 3" ]
            1, 0, [ "item 1" ; "item 0"; "item 2"; "item 3"; "item 4" ]
            2, 1, [ "item 0" ; "item 2"; "item 1"; "item 3"; "item 4" ]
            3, 2, [ "item 0" ; "item 1"; "item 3"; "item 2"; "item 4" ]
            4, 3, [ "item 0" ; "item 1"; "item 2"; "item 4"; "item 3" ]

            0, 2, [ "item 1"; "item 2"; "item 0"; "item 3"; "item 4" ]
            4, 2, [ "item 0"; "item 1"; "item 4"; "item 2"; "item 3" ]
        ] |> List.map (fun (si, ei, expected) ->
            testCase (sprintf "Single List Swap - Swapping %i to %i produces %A" si ei expected) <| fun _ ->
                let output = singleSwap si ei [singleList]
                Expect.equal output [expected] "Did not get expected result"
        )

    let multiList = [
        [ "Item 0 0"; "Item 0 1" ; "Item 0 2"; "Item 0 3"]
        [ "Item 1 0"; "Item 1 1"; "Item 1 2"; "Item 1 3"]
    ]

    let multiSwap (sli, si) (eli, ei) lis = DragAndDrop3.moveItem2 (sli, si) (eli, ei) lis

    let multiListTestBuilder() =
        [
            (1, 0), (0, 0), [
                [ "Item 1 0"; "Item 0 0"; "Item 0 1" ; "Item 0 2"; "Item 0 3"]
                [ "Item 1 1"; "Item 1 2"; "Item 1 3"]
            ]

            (0, 0), (0, 0), [
                [ "Item 0 0"; "Item 0 1" ; "Item 0 2"; "Item 0 3"]
                [ "Item 1 0"; "Item 1 1"; "Item 1 2"; "Item 1 3"]
            ]

            (0, 0), (1, 0), [
                [ "Item 0 1" ; "Item 0 2"; "Item 0 3"]
                [ "Item 0 0"; "Item 1 0"; "Item 1 1"; "Item 1 2"; "Item 1 3"]
            ]

            (0, 1), (1, 3), [
                [ "Item 0 0";  "Item 0 2"; "Item 0 3"]
                [ "Item 1 0"; "Item 1 1"; "Item 1 2"; "Item 0 1"; "Item 1 3"]
            ]

            (0, 1), (1, 4), [
                [ "Item 0 0";  "Item 0 2"; "Item 0 3"]
                [ "Item 1 0"; "Item 1 1"; "Item 1 2"; "Item 1 3"; "Item 0 1"; ]
            ]
        ] |> List.map (fun (x, y, expected) ->
            testCase (sprintf "Multi List Swap - Swapping %A to %A" x y) <| fun _ ->
                let output = multiSwap x y multiList
                Expect.equal output expected "Did not get expected result"
        )


    [<Tests>]
    let tests =
        testList "Drag and drop 3 move item 2 tests" [
            testCase "Moving an item to the same index does not change the list" <| fun _ ->
                let output = singleSwap 0 0 [singleList]
                Expect.equal output [singleList] "Should not alter the list"
            testCase "moving an item to the same index at the last index does not change the list" <| fun _ ->
                let output = singleSwap 4 4 [singleList]
                Expect.equal output [singleList] "Should not alter the list"
            yield! testBuilder()
            yield! multiListTestBuilder()
        ]


// *************
// tests below this line target the original code
// *************

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