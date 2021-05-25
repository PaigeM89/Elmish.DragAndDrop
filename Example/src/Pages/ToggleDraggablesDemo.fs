namespace Pages

(*
This demo shows a way to toggle draggability on some elements, 
and how the other elements interact with those draggable & 
non-draggable elements.
*)

module ToggleDraggablesDemo =
  open Fable.React
  open Fable.React.Props
  open Elmish
  open Elmish.DragAndDrop

  type ContentType =
  | Heading of text : String
  | Paragraph of text : string

  type ContentValue = ContentType

  type Model = {
    DragAndDrop : DragAndDropModel
    ContentMap : Map<string, ContentValue>
  }

  let init() = {
    DragAndDrop = DragAndDropModel.Empty()
    ContentMap = Map.empty
  }