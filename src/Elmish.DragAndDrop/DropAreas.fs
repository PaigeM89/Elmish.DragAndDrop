namespace Elmish.DragAndDrop

open Elmish
open Elmish.DragAndDropHelpers
open Elmish.DragAndDropHelpers.HelperTypes
open Elmish.Throttle
open Fable.React
open Fable.React.Props
open Elmish.DragAndDrop

[<AutoOpen>]
module DropAreas =

  let drawPlaceholder { Styles = styles; Props = props; Content = content } =
    let handle = Draggable.SelfHandle 
    []

  type DropArea =
    static member DropArea model (categoryKey: DragAndDropCategoryKey) config (mouseFuncs : MouseEventHandlers) dispatch id tag props content =
      match model.Moving with
      | None ->
        tag props content
      | Some { StartLocation = (key, _, _, draggedElementId ) } ->
        if key = categoryKey then
          let hoverFunc : MouseEventWithThrottle =
            fun ev throttleId ->
              mouseFuncs.GetOnHoverEnter() ev draggedElementId throttleId
          let listeners  : IHTMLProp list = [
            Listeners.nonDraggableHoverListenerWithFunc model categoryKey id dispatch hoverFunc config.MoveThrottleTimeMs
            Listeners.releaseListenerWithFunc categoryKey (mouseFuncs.GetOnDrop()) draggedElementId dispatch
          ]
          let props = props @ listeners
          let content =
            match config.Placeholder with
            | Some { Styles = phStyles; Props = phProps; Content = phContent } -> 
              let phId = id + "-placeholder"
              let phProps = phProps @ [ Id phId ]
              let handle = Draggable.SelfHandle model categoryKey config dispatch phId div phStyles phProps phContent
              content @ handle
            | None -> content 
          tag props content
        else
          tag props content


  // ************************************************************************************
  // DRAG AND DROP CONTEXT
  // ************************************************************************************

  type DragDropContext =
    static member Context model dispatch tag (props : IHTMLProp list) content =
      match model.Moving with
      | None ->
        tag props content
      | Some ({ StartLocation = (key, _, _, _)}) ->
        let props = [
          yield! props
          Listeners.defaultReleaseListener key dispatch
          Listeners.defaultMouseMoveListener key dispatch
        ]
        tag props content

    // static member MultipleModels (models : DragAndDropModel list) dispatch (tag : Tag) (props : Props) content =
    //   let anyMoving = models |> List.choose (fun m -> m.Moving) |> List.tryHead
    //   match anyMoving with
    //   | Some { CategoryKey = key } ->
    //     let props = [
    //       yield! props
    //       Listeners.defaultReleaseListener key dispatch
    //       Listeners.defaultMouseMoveListener key dispatch
    //     ]
    //     tag props content
    //   | None ->
    //     tag props content

  // ************************************************************************************
  // UPDATE
  // ************************************************************************************


  module internal ItemMoving =
    open Fable.Core

    let private moveItemSameList listIndex startIndex insertAtIndex listIndexes =
      match Map.tryFind listIndex listIndexes with
      | Some innerList ->
        // if we're inserting an item at a later point in the list...
        if startIndex < insertAtIndex then
          // grab everything up to the start
          let beginning, rest = List.split startIndex innerList
          // grab the middle & end sections
          let middle, _end = List.split (insertAtIndex - startIndex + 1) rest
          // this middle section should be 2 items - split it & swap them
          let x, y = List.split 1 middle
          let newList = beginning @ y @ x @ _end
          Map.add listIndex newList listIndexes
        // otherwise we're inserting at an earlier point in the list...
        elif startIndex > insertAtIndex then
          // grab everything up to the start
          let beginning, rest = List.split insertAtIndex innerList
          // grab the end section
          let middle, _end = List.split (startIndex - insertAtIndex) rest
          let head, tail = List.split 1 _end
          let newList = beginning @ head @ middle @ tail
          Map.add listIndex newList listIndexes
        else
          listIndexes
      | None ->
        JS.console.error("Unreachable state: cannot find list at index", listIndex)
        listIndexes

    // let private moveItemSameList listIndex startIndex insertAtIndex li =
    //   match List.tryItem listIndex li with
    //   | Some innerList ->
    //     // if we're inserting an item at a later point in the list...
    //     if startIndex < insertAtIndex then
    //       // grab everything up to the start
    //       let beginning, rest = List.split startIndex innerList
    //       // grab the middle & end sections
    //       let middle, _end = List.split (insertAtIndex - startIndex + 1) rest
    //       // this middle section should be 2 items - split it & swap them
    //       let x, y = List.split 1 middle
    //       let newList = beginning @ y @ x @ _end
    //       List.replaceAt newList listIndex li
    //     // otherwise we're inserting at an earlier point in the list...
    //     elif startIndex > insertAtIndex then
    //       // grab everything up to the start
    //       let beginning, rest = List.split insertAtIndex innerList
    //       // grab the end section
    //       let middle, _end = List.split (startIndex - insertAtIndex) rest
    //       let head, tail = List.split 1 _end
    //       let newList = beginning @ head @ middle @ tail
    //       List.replaceAt newList listIndex li
    //     else
    //       li
    //   | None ->
    //     JS.console.error("Unreachable state: cannot find list at index", listIndex)
    //     li

    // let moveItem (startListIndex, startIndex) (insertListIndex, insertAtIndex) li =
    //   if startListIndex = insertListIndex then
    //     moveItemSameList startListIndex startIndex insertAtIndex li
    //   else
    //     // lists are not the same; grab the item, insert it to the new list, and remove it from the old list
    //     match List.tryItem startListIndex li, List.tryItem insertListIndex li with
    //     | Some startList, Some insertList ->
    //       match List.tryItem startIndex startList with
    //       | Some item ->
    //         let newStartList = List.removeAt startIndex startList
    //         let newInsertList = List.insertAt item insertAtIndex insertList

    //         li
    //         |> List.replaceAt newStartList startListIndex
    //         |> List.replaceAt newInsertList insertListIndex
    //       | None ->
    //         JS.console.error("Unreachable state: cannot find item in list at index", startListIndex, startIndex)
    //         li
    //     | None, _ ->
    //       JS.console.error("Unreachable state: cannot find list at index", startListIndex)
    //       li
    //     | _, None ->
    //       JS.console.error("Unreachable state: cannot find list at index", insertListIndex)
    //       li
  
    let moveItem (startListIndex, startIndex) (insertListIndex, insertAtIndex) (indexMap : ListIndexTree) =
      if startListIndex = insertListIndex then
        moveItemSameList startListIndex startIndex insertAtIndex indexMap
      else
        // lists are not the same; grab the item, insert it to the new list, and remove it from the old list
        match Map.tryFind startListIndex indexMap, Map.tryFind insertListIndex indexMap with
        | Some startList, Some insertList ->
          match List.tryItem startIndex startList with
          | Some item ->
            let newStartList = List.removeAt startIndex startList
            let newInsertList = List.insertAt item insertAtIndex insertList

            indexMap
            |> Map.add startListIndex newStartList
            |> Map.add insertListIndex newInsertList
            //|> List.replaceAt newStartList startListIndex
            //|> List.replaceAt newInsertList insertListIndex
          | None ->
            JS.console.error("Unreachable state: cannot find item in list at index", startListIndex, startIndex)
            indexMap
        | None, _ ->
          JS.console.error("Unreachable state: cannot find list at index", startListIndex)
          indexMap
        | _, None ->
          JS.console.error("Unreachable state: cannot find list at index", insertListIndex)
          indexMap

  let dragAndDropUpdate msg (model : DragAndDropModel) =
    match msg with
    | DragStart (loc, startCoords, offset) ->
      let categoryKey = locKey loc
      let movingStatus = MovingStatus.Init categoryKey (loc) |> Some
      { model with Moving = movingStatus; Cursor = startCoords; Offset = Some offset }, Cmd.none
    | DragAndDropMsg.OnDrag coords ->
      {model with Cursor = coords }, Cmd.none
    | DragOver (category, listIndex, index, elementId) ->
      //if category = categoryKey then
      match model.Moving with
      | Some { StartLocation = (categoryKey, startList, startIndex, startingElementId) }->
        let slide = None //tryGetSlide elementId
        let mdl =
          DragAndDropModel.getItemsForCategoryOrEmpty categoryKey model
          |> ItemMoving.moveItem (startList, startIndex) (listIndex, index)
          |> denseRankElementIndexes
          |> DragAndDropModel.replaceItemsForCategory categoryKey model
        // let mdl = { model with Items = items' } // |> Model.setSlideOpt slide
        let newStartLoc = (categoryKey, listIndex, index, startingElementId)
        (setDragSource categoryKey newStartLoc mdl), Cmd.none
      | None ->
        model, Cmd.none
      // else
      //   model, Cmd.none

    | DragOverNonDraggable (key, listIndex, dropAreaId) ->
      printfn "Drag Over Non Draggable raised for %A" (listIndex, dropAreaId)
      model, Cmd.none

    | DragEnd ->
      { model with Moving = None; Offset = None }, Cmd.none
    | ThrottleMsg throttleMsg ->
      // let the throttler handle the message
      let throttleResult = handleThrottleMsg throttleMsg model.ThrottleState
      match throttleResult with
      // get back a new state and a command 
      | Ok (throttleState, throttleCmd) ->
          // map the command so it's run
          { model with ThrottleState = throttleState }, Cmd.map ThrottleMsg throttleCmd
      | Error e ->
          //printfn "Error throttling: %A" e
          Fable.Core.JS.console.error("Error throttling: ", e)
          model, Cmd.none

  // let dragAndDropUpdate msg categoryKey (model : DragAndDropModel) =
  //   match msg with
  //   | DragStart (loc, startCoords, offset) ->
  //     let movingStatus = MovingStatus.Init categoryKey (loc) |> Some
  //     { model with Moving = movingStatus; Cursor = startCoords; Offset = Some offset }, Cmd.none
  //   | DragAndDropMsg.OnDrag coords ->
  //     {model with Cursor = coords }, Cmd.none
  //   | DragOver (category, listIndex, index, elementId) ->
  //     if category = categoryKey then
  //       match model.Moving with
  //       | Some { StartLocation = (_, startList, startIndex, startingElementId) }->
  //         let slide = None //tryGetSlide elementId
  //         let mdl =
  //           DragAndDropModel.getItemsForCategoryOrEmpty categoryKey model
  //           |> ItemMoving.moveItem (startList, startIndex) (listIndex, index)
  //           |> denseRankElementIndexes
  //           |> DragAndDropModel.replaceItemsForCategory categoryKey model
  //         // let mdl = { model with Items = items' } // |> Model.setSlideOpt slide
  //         let newStartLoc = (categoryKey, listIndex, index, startingElementId)
  //         (setDragSource categoryKey newStartLoc mdl), Cmd.none
  //       | None ->
  //         model, Cmd.none
  //     else
  //       model, Cmd.none

  //   | DragOver (category, _, _, _) when category <> categoryKey -> model, Cmd.none

  //   | DragOverNonDraggable (key, listIndex, dropAreaId) ->
  //     printfn "Drag Over Non Draggable raised for %A" (listIndex, dropAreaId)
  //     model, Cmd.none

  //   | DragEnd ->
  //     { model with Moving = None; Offset = None }, Cmd.none
  //   | ThrottleMsg throttleMsg ->
  //     // let the throttler handle the message
  //     let throttleResult = handleThrottleMsg throttleMsg model.ThrottleState
  //     match throttleResult with
  //     // get back a new state and a command 
  //     | Ok (throttleState, throttleCmd) ->
  //         // map the command so it's run
  //         { model with ThrottleState = throttleState }, Cmd.map ThrottleMsg throttleCmd
  //     | Error e ->
  //         //printfn "Error throttling: %A" e
  //         Fable.Core.JS.console.error("Error throttling: ", e)
  //         model, Cmd.none

  // let dragAndDropMultipleModelsUpdate msg (models : DragAndDropModel list) =
  //   match msg with
  //   | DragStart (loc, startCoords, offset) ->
  //     let movingStatus = MovingStatus.Init (loc) |> Some
  //     { model with Moving = movingStatus; Cursor = startCoords; Offset = Some offset }, Cmd.none
  //   | DragAndDropMsg.OnDrag coords ->
  //     {model with Cursor = coords }, Cmd.none
  //   | DragOver (listIndex, index, elementId) ->
  //     match model.Moving with
  //     | Some { StartLocation = (startList, startIndex, startingElementId) }->
  //       let slide = None //tryGetSlide elementId
  //       let items' =
  //         ItemMoving.moveItem (startList, startIndex) (listIndex, index) model.Items
  //         |> getUpdatedItemLocations
  //       let mdl = { model with Items = items' } // |> Model.setSlideOpt slide
  //       let newStartLoc = (listIndex, index, startingElementId)
  //       (setDragSource newStartLoc mdl), Cmd.none
  //     | None ->
  //       model, Cmd.none

  //   | DragOverNonDraggable (listIndex, dropAreaId) ->
  //     printfn "Drag Over Non Draggable raised for %A" (listIndex, dropAreaId)
  //     model, Cmd.none

  //   | DragEnd ->
  //     { model with Moving = None; Offset = None }, Cmd.none
  //   | ThrottleMsg throttleMsg ->
  //     // let the throttler handle the message
  //     let throttleResult = handleThrottleMsg throttleMsg model.ThrottleState
  //     match throttleResult with
  //     // get back a new state and a command 
  //     | Ok (throttleState, throttleCmd) ->
  //         // map the command so it's run
  //         { model with ThrottleState = throttleState }, Cmd.map ThrottleMsg throttleCmd
  //     | Error e ->
  //         //printfn "Error throttling: %A" e
  //         Fable.Core.JS.console.error("Error throttling: ", e)
  //         model, Cmd.none
