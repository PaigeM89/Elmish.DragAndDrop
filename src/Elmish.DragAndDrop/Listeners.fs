namespace Elmish.DragAndDrop2

open Elmish
open Elmish.DragAndDropHelpers
open Elmish.DragAndDropHelpers.HelperTypes
open Elmish.Throttle
open Fable.React
open Fable.React.Props

[<AutoOpen>]
module internal Listeners =
  open Browser.Types
  open Elmish.DragAndDropHelpers.BrowserHelpers
  open Fable.Core
  open Elmish.DragAndDrop2

  // /// Listens for a "MouseDown" event on an element
  // let defaultDraggable model (draggableId : DraggableId) dispatch =
  //   let loc = getLocationForElement draggableId model
  //   match loc with
  //   | Some loc ->
  //     OnMouseDown (fun (ev : Browser.Types.MouseEvent) ->
  //       printfn "On Mouse Down: %A" ev
  //       ev.preventDefault()
  //       let o = getOffset ev (locId loc)
  //       (loc, fromME ev, o) |> DragStart |> dispatch
  //     )
  //   | None -> 
  //     JS.console.error(sprintf "No location found for element with id '%s' in drag and drop items" draggableId)
  //     OnMouseDown (fun (ev : Browser.Types.MouseEvent) -> ())

  /// Listens for a "mouse down" event. On an event, this returns the given id, which is
  /// _the id of the element to move_, which does not have to be the id of the element being clicked.
  /// For example, this could be the id of a parent element, where one of the child elements is a "handle".
  let defaultMouseDownListener model (draggableId : DraggableId) dispatch =
    let loc = getLocationForElement draggableId model
    match loc with
    | Some loc ->
      OnMouseDown (fun (ev : Browser.Types.MouseEvent) ->
        printfn "On Mouse Down: %A" ev
        ev.preventDefault()
        let o = getOffset ev (locId loc)
        (loc, fromME ev, o) |> DragStart |> dispatch
      )
    | None -> 
      JS.console.error(sprintf "No location found for element with id '%s' in drag and drop items" draggableId)
      OnMouseDown (fun (ev : Browser.Types.MouseEvent) -> ())

  let defaultMouseMoveListener dispatch =
    OnMouseMove (fun (ev : MouseEvent) ->
      ev.preventDefault()
      let c = coords ev.clientX ev.clientY
      DragAndDropMsg.OnDrag c |> dispatch
    )

  /// Listener for when another element is being dragged and is moved over this element.
  let defaultHoverListener model id dispatch throttleTimeSpan =
    OnMouseEnter (fun (ev : MouseEvent) ->
      ev.preventDefault()
      printfn "on mouse enter for element %A" id
      match throttleTimeSpan with
      | Some timespan ->
          let isThrottled = throttle model.ThrottleState id timespan ev
          match isThrottled with
          | None -> ()
          | Some (ev, throttleMsg) ->
              throttleMsg |> ThrottleMsg |> dispatch
              let loc = getLocationForElement id model
              match loc with
              | Some loc ->
                  DragOver (loc) |> dispatch
              | None -> ()
      | None ->
          let loc = getLocationForElement id model
          match loc with
          | Some loc ->
              printfn "Dragging element over location %A" loc
              DragOver (loc) |> dispatch
          | None -> ()
    )

  let hoverListenerWithFunc model id dispatch (func : MouseEventWithThrottle) throttleTimeSpan =
    OnMouseEnter (fun (ev : MouseEvent) ->
      ev.preventDefault()
      match throttleTimeSpan with
      | Some timespan ->
          let isThrottled = throttle model.ThrottleState id timespan ev
          match isThrottled with
          | None -> ()
          | Some (ev, throttleMsg) ->
              throttleMsg |> ThrottleMsg |> dispatch
              func ev id
              let loc = getLocationForElement id model
              match loc with
              | Some loc ->
                  DragOver (loc) |> dispatch
              | None -> ()
      | None ->
          func ev id
          let loc = getLocationForElement id model
          match loc with
          | Some loc ->
              DragOver (loc) |> dispatch
          | None -> ()
    )

  let hoverLeaveListenerWithFunc model id dispatch (func : MouseEventWithThrottle) throttleTimeSpan =
    OnMouseLeave (fun (ev : MouseEvent) ->
      ev.preventDefault()
      match throttleTimeSpan with
      | Some timespan ->
          let isThrottled = throttle model.ThrottleState id timespan ev
          match isThrottled with
          | None -> ()
          | Some (ev, throttleMsg) ->
              throttleMsg |> ThrottleMsg |> dispatch
              func ev id
              let loc = getLocationForElement id model
              match loc with
              | Some loc ->
                  DragOver (loc) |> dispatch
              | None -> ()
      | None ->
          func ev id
          let loc = getLocationForElement id model
          match loc with
          | Some loc ->
              DragOver (loc) |> dispatch
          | None -> ()
    )

  let nonDraggableHoverListenerWithFunc model id dispatch (func : MouseEventWithThrottle) throttleTimeSpan =
    OnMouseEnter (fun (ev : MouseEvent) ->
      ev.preventDefault()
      match throttleTimeSpan with
      | Some timespan ->
          let isThrottled = throttle model.ThrottleState id timespan ev
          match isThrottled with
          | None -> ()
          | Some (ev, throttleMsg) ->
              throttleMsg |> ThrottleMsg |> dispatch
              func ev id
              let loc = getLocationForElement id model
              match loc with
              | Some (listIndex, _, _) ->
                  DragOverNonDraggable (id, Some listIndex) |> dispatch
              | None -> DragOverNonDraggable (id, None) |> dispatch
      | None ->
          func ev id
          let loc = getLocationForElement id model
          match loc with
          | Some (listIndex, _, _) ->
              DragOverNonDraggable (id, Some listIndex) |> dispatch
          | None -> DragOverNonDraggable (id, None) |> dispatch
    )

  let defaultReleaseListener dispatch =
    OnMouseUp (fun (ev : MouseEvent) -> ev.preventDefault(); DragEnd |> dispatch)

  let releaseListenerWithFunc func elementId dispatch =
    OnMouseUp (fun (ev : MouseEvent) -> 
      ev.preventDefault()
      func ev elementId
      DragEnd |> dispatch
    )