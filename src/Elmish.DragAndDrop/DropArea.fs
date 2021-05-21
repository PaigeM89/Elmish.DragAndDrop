namespace Elmish.DragAndDrop

[<AutoOpen>]
module DropArea =
  open Elmish
  open Fable.React
  open Fable.React.Props
  open Elmish.DragAndDrop.Types
  open Elmish.DragAndDrop.Helpers
  open Elmish.DragAndDrop.Helpers.HelperTypes

  type DropArea =
    static member fromGenerators model dispatch config props content =
      match model.Moving with
      | None ->
        let children =
          content
          |> List.map (fun (elementId : ElementId, gen : ElementGenerator) -> 
            renderDraggable DragStatus.NoActiveDrag model config elementId dispatch gen
          )
        div props children
      | Some { StartLocation = (listIndex, index, draggedElementId) } ->
        let props = [
          yield! props
          Listeners.defaultReleaseListener dispatch
          Listeners.defaultMouseMoveListener dispatch
        ]
        let children =
          content
          |> List.map (fun (elementId, gen) ->
            renderDraggable (DragStatus.ActiveDrag draggedElementId) model config elementId dispatch gen
          )
        div props children

    static member fromDragHandles model dispatch config props content =
      match model.Moving with
      | None ->
        let children =
          content
          |> List.map (fun (elementId : ElementId, handle : DragHandle) -> 
            renderDragHandle DragStatus.NoActiveDrag model config elementId dispatch handle
          )
        div props children
      | Some { StartLocation = (listIndex, index, draggedElementId) } ->
        let props = [
          yield! props
          Listeners.defaultReleaseListener dispatch
          Listeners.defaultMouseMoveListener dispatch
        ]
        let children =
          content
          |> List.map (fun (elementId, handle) ->
            renderDragHandle (DragStatus.ActiveDrag draggedElementId) model config elementId dispatch handle
          )
        div props children

