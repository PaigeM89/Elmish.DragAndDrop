namespace Elmish.DragAndDrop2

open Elmish
open Elmish.DragAndDropHelpers
open Elmish.DragAndDropHelpers.HelperTypes
open Elmish.Throttle
open Fable.React
open Fable.React.Props
open Elmish.DragAndDrop2

[<AutoOpen>]
module Draggables = 
  let private orEmpty li = Option.defaultValue [] li

  type DragHandle =
    static member Handle model (id : DraggableId) dispatch tag props content =
      match model.Moving with
      | None ->
        // no active drag; add a listener for a mouse down event
        let listener = (Listeners.defaultMouseDownListener model id dispatch) :> IHTMLProp
        // add this listener last so it overrides any instances of the same prop.
        tag (props @ [listener]) content
      | Some _ ->
        // if there is an active drag, then we don't need the listener.
        tag props content

  let buildHoverPreview config id (tag : Tag) styles props content =
    let styles : IHTMLProp =
      styles @ (config.HoverPreviewElementStyles |> orEmpty)
      |> Style
      :> IHTMLProp
    let props = props @ ( config.HoverPreviewElementProperties |> orEmpty)
    tag (styles :: props) content

  let buildDragged config model id tag styles props content =
    let cursor = model.Cursor
    let draggedStyles =
      (config.DraggedElementStyles |> orEmpty) @ ([
        PointerEvents "none"
        Left cursor.x
        Top cursor.y
      ])
    let style = (styles @ draggedStyles) |> Style :> IHTMLProp
    let props = props @ (config.DraggedElementProperties |> orEmpty)
    tag (style :: props) content

  type Draggable =
    static member SelfHandle model config dispatch id (tag : Tag) styles props content =
      match model.Moving with
      | None ->
        // no drag in progress, render with mouse down listener
        let props : IHTMLProp list = ((Style styles) :> IHTMLProp) :: props 
        let handle = DragHandle.Handle model id dispatch tag props content
        [ handle ]
      | Some { StartLocation = (_, _, draggedElementId )} ->
        if id = draggedElementId then
          let preview = buildHoverPreview config id tag styles props content
          let dragged = buildDragged config model id tag styles props content
          [ preview; dragged ]
        else
          let listener = Listeners.defaultHoverListener model id dispatch config.MoveThrottleTimeMs :> IHTMLProp
          let styles = (config.ListenerElementStyles |> orEmpty) @ styles |> Style :> IHTMLProp
          // added in this order specifically; listener overrides props overrides config
          let props = (config.ListenerElementProperties |> orEmpty) @ props @ [ listener ]
          [tag (styles :: props) content]

    static member Draggable model config dispatch id (tag : Tag) styles props content =
      ()