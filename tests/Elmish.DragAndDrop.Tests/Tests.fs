namespace Elmish.DragAndDrop.Tests

open System
open Elmish
open Expecto

module ListTests =
    open Elmish.DragAndDrop.Helpers.List

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

module ModelTests =
  open Elmish.DragAndDrop

  let items =  [
    [
      "element-0-0"
      "element-0-1"
      "element-0-2"
    ]
    [
      "element-1-0"
      "element-1-1"
      "element-1-2"
    ]
    [
      "element-2-0"
      "element-2-1"
      "element-2-2"
    ]
  ]

  let model = Model.createWithItemsMultiList items

  [<Tests>]
  let tests = testList "Model tests" [
    testList "Sanity checks" [
      testCase "Model created with 0 items contains a single empty list" <| fun _ ->
        let model = Model.createWithItems []
        Expect.hasLength model.Items 1 "Should have a single list"
        Expect.hasLength (model.Items.[0]) 0 "Should have an empty list at 0th index"
      testCase "model created with multiple lists from an empty list is completely empty" <| fun _ ->
        let model = Model.createWithItemsMultiList []
        Expect.hasLength model.Items 0 "Should not have any items"
      // testCase "initItemLocations does not map over an empty collection" <| fun _ ->
      //   let output = initItemLocations []
      //   Expect.hasLength output 0 "should have 0 length"
    ]
    testList "insertNewItem tests" [
      testCase "inserting new item to first spot of empty collection inserts that item" <| fun _ ->
        let model = Model.createWithItems []
        let item = "new-item"
        let output = Model.insertNewItemAt 0 0 item model
        Expect.hasLength output.Items.[0] 1 "Should have insert item at 0th index"
        let (li, i, id) = output.Items.[0].[0]
        Expect.equal id item "Should have the correct item id"
      testCase "inserting new item to head of list of empty collection inserts that item" <| fun _ ->
        let model = Model.createWithItems []
        let item = "new-item"
        let output = Model.insertNewItemAtHead 0 item model
        Expect.hasLength output.Items.[0] 1 "Should have insert item at 0th index"
        let (li, i, id) = output.Items.[0].[0]
        Expect.equal id item "Should have the correct item id"

      testCase "Inserting new item to non-existent list index creates a new list" <| fun _ ->
        let model = Model.createWithItemsMultiList []
        let item = "new-item"
        let output = Model.insertNewItemAt 1 0 item model
        Expect.hasLength output.Items.[0] 1 "Should have insert item at 0th index"
        let (li, i, id) = output.Items.[0].[0]
        Expect.equal id item "Should have the correct item id"

      testCase "inserting new item to head of list of populated collection inserts that item" <| fun _ ->
        let model = Model.createWithItemsMultiList items
        let item = "new-item"
        let output = Model.insertNewItemAtHead 0 item model
        Expect.hasLength output.Items.[0] 4 "Should have insert item at 0th index"
        let (li, i, id) = output.Items.[0].[0]
        Expect.equal id item "Should have the correct item id"
      testCase "inserting new item at middle of list inserts that item" <| fun _ ->
        let model = Model.createWithItemsMultiList items
        let item = "new-item"
        let output = Model.insertNewItemAt 0 2 item model
        let (li, i, id) = output.Items.[0].[2]
        Expect.equal id item "Should have the correct item id"
        let (_, _, id2) = output.Items.[0].[3]
        Expect.equal id2 "element-0-2" "Should have moved existing item to 3rd index"
      testCase "inserting new item middle list, middle of collection inserts that item" <| fun _ ->
        let model = Model.createWithItemsMultiList items
        let item = "new-item"
        let output = Model.insertNewItemAt 1 1 item model
        Expect.hasLength output.Items.[1] 4 "Should have 4 items"
        let (li, i, id) = output.Items.[1].[1]
        Expect.equal id item "Should have the correct item id"
        let (_, _, id2) = output.Items.[1].[2]
        Expect.equal id2 "element-1-1" "Should have moved existing item to 2rd index"
    ]
    testList "removeItem tests" [
      testCase "Remove Item At on empty collection returns empty collection" <| fun _ ->
        let model = Model.createWithItems []
        let output = removeItemAt 0 0 model
        Expect.hasLength output.Items.[0] 0 "Should not have any items in collection"
      testCase "Remove Item returns empty collection when given empty collection" <| fun _ ->
        let model = Model.createWithItems []
        let output = removeItem "hello-world" model
        Expect.hasLength output.Items.[0] 0 "Should not have any items in collection"

      testCase "Remove Item At removes item in middle of collection" <| fun _ ->
        let model = Model.createWithItemsMultiList items
        let output = removeItemAt 1 1 model
        Expect.hasLength output.Items 3 "Should contain 3 lists of items"
        Expect.hasLength output.Items.[1] 2 "Should have 2 items in collection with item removed"
      testCase "Remove Item removes item by Id in the middle of a collection" <| fun _ ->
        let model = Model.createWithItemsMultiList items
        let output = removeItem "element-1-1" model
        Expect.hasLength output.Items 3 "Should contain 3 lists of items"
        Expect.hasLength output.Items.[1] 2 "Should have 2 items in collection with item removed"

      testCase "Remove Item At returns collection if target is outside collection range" <| fun _ ->
        let model = Model.createWithItemsMultiList items
        let output = removeItemAt 3 1 model
        Expect.hasLength model.Items 3 "Should contain 3 lists of items"
        for li in output.Items do
          Expect.hasLength li 3 "List should not have any items removed"
      testCase "Remove Item returns collection if target id is not in the collection" <| fun _ ->
        let model = Model.createWithItemsMultiList items
        let output = removeItem "hello-world" model
        Expect.hasLength model.Items 3 "Should contain 3 lists of items"
        for li in output.Items do
          Expect.hasLength li 3 "List should not have any items removed"
    ]
    testList "Replace Item tests" [
      testCase "replacing item in empty collection simply inserts that item" <| fun _ ->
        let model = Model.createWithItemsMultiList []
        let output = replaceItemAt 0 1 "new-item" model
        Expect.hasLength output.Items 1 "Should create a list"
        Expect.hasLength output.Items.[0] 1 "Should insert the item"
      testCase "replacing item in collection replaces that item" <| fun _ -> 
        let model = Model.createWithItemsMultiList items
        let output = replaceItemAt 1 1 "new-item" model
        Expect.hasLength output.Items 3 "Should contain 3 lists of items"
        Expect.hasLength output.Items.[1] 3 "Should have 2 items in collection with item removed"
        Expect.equal output.Items.[1].[1] (1, 1, "new-item") "Should replace item at correct index"
    ]
  ]