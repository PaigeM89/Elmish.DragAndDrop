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
