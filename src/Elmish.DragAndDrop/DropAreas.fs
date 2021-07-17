namespace Elmish.DragAndDrop2

open Elmish
open Elmish.DragAndDropHelpers
open Elmish.DragAndDropHelpers.HelperTypes
open Elmish.Throttle
open Fable.React
open Fable.React.Props
open Elmish.DragAndDrop2

[<AutoOpen>]
module DropAreas =

  type DropAreaId = string
  type OnHover = Browser.Types.MouseEvent -> DraggableId -> Throttle.Id -> unit
  type OnDrop  = Browser.Types.MouseEvent -> DraggableId -> unit

  type DropArea =
    static member DropArea model config (onHover : OnHover) (onDrop : OnDrop) dispatch tag props content =
      match model.Moving with
      | None ->
        tag props content
      | Some { StartLocation = (_, _, draggedElementId ) } ->
        let hoverFunc ev throttleId = onHover ev draggedElementId throttleId
        let listeners  : IHTMLProp list = [
          Listeners.hoverListenerWithFunc model draggedElementId dispatch hoverFunc config.MoveThrottleTimeMs
          Listeners.releaseListenerWithFunc onDrop draggedElementId dispatch
        ]
        let props = props @ listeners
        tag props content


  // ************************************************************************************
  // DRAG AND DROP CONTEXT
  // ************************************************************************************

  type DragDropContext =
    static member context model dispatch tag (props : IHTMLProp list) content =
      match model.Moving with
      | None ->
        tag props content
      | Some _ ->
        let props = [
          yield! props
          Listeners.defaultReleaseListener dispatch
          Listeners.defaultMouseMoveListener dispatch
        ]
        tag props content

  // ************************************************************************************
  // UPDATE
  // ************************************************************************************


  module internal ItemMoving =
    open Fable.Core

    let private moveItemSameList listIndex startIndex insertAtIndex li =
      match List.tryItem listIndex li with
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
          List.replaceAt newList listIndex li
        // otherwise we're inserting at an earlier point in the list...
        elif startIndex > insertAtIndex then
          // grab everything up to the start
          let beginning, rest = List.split insertAtIndex innerList
          // grab the end section
          let middle, _end = List.split (startIndex - insertAtIndex) rest
          let head, tail = List.split 1 _end
          let newList = beginning @ head @ middle @ tail
          List.replaceAt newList listIndex li
        else
          li
      | None ->
        JS.console.error("Unreachable state: cannot find list at index", listIndex)
        li

    let moveItem (startListIndex, startIndex) (insertListIndex, insertAtIndex) li =
      if startListIndex = insertListIndex then
        moveItemSameList startListIndex startIndex insertAtIndex li
      else
        // lists are not the same; grab the item, insert it to the new list, and remove it from the old list
        match List.tryItem startListIndex li, List.tryItem insertListIndex li with
        | Some startList, Some insertList ->
          match List.tryItem startIndex startList with
          | Some item ->
            let newStartList = List.removeAt startIndex startList
            let newInsertList = List.insertAt item insertAtIndex insertList

            li
            |> List.replaceAt newStartList startListIndex
            |> List.replaceAt newInsertList insertListIndex
          | None ->
            JS.console.error("Unreachable state: cannot find item in list at index", startListIndex, startIndex)
            li
        | None, _ ->
          JS.console.error("Unreachable state: cannot find list at index", startListIndex)
          li
        | _, None ->
          JS.console.error("Unreachable state: cannot find list at index", insertListIndex)
          li
  
  let dragAndDropUpdate msg model =
    match msg with
    | DragStart (loc, startCoords, offset) ->
      let movingStatus = MovingStatus.Init (loc) |> Some
      { model with Moving = movingStatus; Cursor = startCoords; Offset = Some offset }, Cmd.none
    | DragAndDropMsg.OnDrag coords ->
      {model with Cursor = coords }, Cmd.none
    | DragOver (listIndex, index, elementId) ->
      match model.Moving with
      | Some { StartLocation = (startList, startIndex, startingElementId) }->
        let slide = None //tryGetSlide elementId
        let items' =
          ItemMoving.moveItem (startList, startIndex) (listIndex, index) model.Items
          |> getUpdatedItemLocations
        let mdl = { model with Items = items' } // |> Model.setSlideOpt slide
        let newStartLoc = (listIndex, index, startingElementId)
        (setDragSource newStartLoc mdl), Cmd.none
      | None ->
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
