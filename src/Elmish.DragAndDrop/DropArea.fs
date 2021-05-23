namespace Elmish.DragAndDrop

[<AutoOpen>]
module DropArea =
  open Elmish
  open Fable.React
  open Fable.React.Props
  open Elmish.DragAndDrop.Types
  open Elmish.DragAndDropHelpers
  open Elmish.DragAndDropHelpers.HelperTypes

  type DropArea =
    static member fromGenerators model dispatch config props content =
      match model.Moving with
      | None ->
        let children =
          content
          |> List.map (fun (elementId, gen) ->
            toDraggables DragStatus.NoActiveDrag model config elementId dispatch gen
          )
          |> List.concat
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
            toDraggables (DragStatus.ActiveDrag draggedElementId) model config elementId dispatch gen
          )
          |> List.concat

        div props children
    
    static member fromGeneratorsWithTag model dispatch config (props: IHTMLProp list) content tag =
      match model.Moving with
      | None ->
        let children =
          content
          |> List.map (fun (elementId, gen) ->
            toDraggables DragStatus.NoActiveDrag model config elementId dispatch gen
          )
          |> List.concat
        tag (Seq.ofList props) (Seq.ofList children)
      | Some { StartLocation = (listIndex, index, draggedElementId) } ->
        let props = [
          yield! props
          Listeners.defaultReleaseListener dispatch
          Listeners.defaultMouseMoveListener dispatch
        ]
        let children =
          content
          |> List.map (fun (elementId, gen) ->
            toDraggables (DragStatus.ActiveDrag draggedElementId) model config elementId dispatch gen
          )
          |> List.concat
        tag (Seq.ofList props) (Seq.ofList children)

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

