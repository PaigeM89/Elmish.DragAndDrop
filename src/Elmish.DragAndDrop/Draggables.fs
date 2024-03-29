namespace Elmish.DragAndDrop

open Elmish
open Elmish.DragAndDropHelpers
open Elmish.DragAndDropHelpers.HelperTypes
open Elmish.Throttle
open Fable.React
open Fable.React.Props
open Elmish.DragAndDrop

[<AutoOpen>]
module Draggables = 
  let private orEmpty li = Option.defaultValue [] li
  let private idProp id = Id id :> IHTMLProp

  type DragHandle =
    static member Handle model (categoryKey : DragAndDropCategoryKey) (id : DraggableId) dispatch tag props content =
      match model.Moving with
      | None ->
        // no active drag; add a listener for a mouse down event
        let listener = (Listeners.defaultMouseDownListener model categoryKey id dispatch) :> IHTMLProp
        // add this listener last so it overrides any instances of the same prop.
        tag (props @ [listener]) content
      | Some _ ->
        // if there is an active drag, then we don't need the listener.
        tag props content

  let buildHoverPreview config id (tag : Tag) styles props content =
    let id = id + "-preview"
    let styles : IHTMLProp =
      styles @ (config.HoverPreviewElementStyles |> orEmpty)
      |> Style
      :> IHTMLProp
    let props = ([idProp id]) @ props @ ( config.HoverPreviewElementProperties |> orEmpty)
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
    let props = ([idProp id]) @ props @ (config.DraggedElementProperties |> orEmpty)
    tag (style :: props) content

  type Draggable =
    static member SelfHandle model (categoryKey : DragAndDropCategoryKey) config dispatch id (tag : Tag) styles props content =
      match model.Moving with
      | None ->
        // no drag in progress, render with mouse down listener
        let props : IHTMLProp list = ((Style styles) :> IHTMLProp) :: props 
        let handle = DragHandle.Handle model categoryKey id dispatch tag props content
        [ handle ]
      | Some { StartLocation = (category, _, _, draggedElementId )} ->
        if category = categoryKey && id = draggedElementId then
          let preview = buildHoverPreview config id tag styles props content
          let dragged = buildDragged config model id tag styles props content
          [ preview; dragged ]
        elif category = categoryKey then
          let listener = Listeners.defaultHoverListener model categoryKey id dispatch config.MoveThrottleTimeMs :> IHTMLProp
          let styles = (config.ListenerElementStyles |> orEmpty) @ styles |> Style :> IHTMLProp
          // added in this order specifically; listener overrides props overrides config
          let props = (config.ListenerElementProperties |> orEmpty) @ props @ [ listener ]
          [tag (styles :: props) content]
        else
          let props = Style styles :> IHTMLProp :: props
          [tag props content]

    static member InnerHandle model (categoryKey : DragAndDropCategoryKey) config dispatch id (tag : Tag) styles props content =
      match model.Moving with
      | None ->
        //no drags in progress, render the content
        let props : IHTMLProp list = [ yield! props; Style styles ]
        [tag props content]
      | Some { StartLocation = (category, _, _, draggedElementId )} ->
        if categoryKey = category && id = draggedElementId then
          let preview = buildHoverPreview config id tag styles props content
          let dragged = buildDragged config model id tag styles props content
          [ preview; dragged ]
        elif categoryKey = category then
          let listener = Listeners.defaultHoverListener model categoryKey id dispatch config.MoveThrottleTimeMs :> IHTMLProp
          let styles = (config.ListenerElementStyles |> orEmpty) @ styles |> Style :> IHTMLProp
          // added in this order specifically; listener overrides props overrides config
          let props = (config.ListenerElementProperties |> orEmpty) @ props @ [ listener ]
          [tag (styles :: props) content]
        else
          let props = Style styles :> IHTMLProp :: props
          [tag props content]
