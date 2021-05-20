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
    static member dropArea model dispatch config props content =
      match model.Moving with
      | None ->
        let children =
          content
          |> List.map (fun (elementId : ElementId, gen : ElementGenerator) -> 
            renderDraggable DragStatus.NoActiveDrag model config elementId dispatch gen
          )
          //|> List.map (fun (eleDispatch, handle) -> render NoActiveDrag model config eleDispatch handle )
        div props children
      | _ -> div props []
