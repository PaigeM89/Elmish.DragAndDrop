namespace Elmish.DragAndDrop

[<AutoOpen>]
module DragHandle =
  open Types
  open Fable.React
  open Fable.React.Props
  open Elmish.DragAndDrop.Helpers
  open Elmish.DragAndDrop.Helpers.HelperTypes

  module internal Rendering =
    let private orEmpty li = Option.defaultValue [] li
    // because property ordering is important, this reverses the append on a list
    let private appendR li1 li2 = li2 @ li1

    /// Renders a handle, a collection of elements with a drag listener.
    let renderHandle mdl id dispatch gen =
      let listener = (Listeners.defaultDraggable mdl id dispatch) :> IHTMLProp
      gen
      |> ElementGenerator.addProps [listener]
      |> ElementGenerator.render

    let renderDragged config cursor id gen =
      let idProp = (Id id) :> IHTMLProp
      let appendedStyles = 
        config.DraggedElementStyles |> orEmpty |> appendR [
          PointerEvents "none"
          Left cursor.x
          Top cursor.y
        ]
      let props = config.DraggedElementProperties |> orEmpty |> appendR [idProp]
      gen |> ElementGenerator.renderWith appendedStyles props

    let renderHoverPreview config id gen =
      let idProp = (Id id) :> IHTMLProp
      let styles = config.HoverPreviewElementStyles |> orEmpty
      let props = config.HoverPreviewElementProperties |> orEmpty |> appendR [ idProp ]
      gen |> ElementGenerator.renderWith styles props
    
    let renderWithHoverListener config model id dispatch gen =
      let listener = Listeners.defaultHoverListener model id dispatch
      let styles = config.ListenerElementStyles |> orEmpty
      let props = config.ListenerElementProperties |> orEmpty |> appendR [listener]
      gen |> ElementGenerator.renderWith styles props

  let internal renderDraggable dragStatus model config id dispatch (gen : ElementGenerator) =
    match dragStatus with
    | NoActiveDrag ->
      //render item as a draggable
      //a Generator should already have a handle defined in it (or is a handle itself).
      gen.Render()
    | ActiveDrag draggedElementId ->
      if id = draggedElementId then
        div [] [
          Rendering.renderDragged config model.Cursor id gen
          Rendering.renderHoverPreview config id gen
        ]
      else
        Rendering.renderWithHoverListener config model id dispatch gen

  type DragHandle = {
    Generator : ElementGenerator
    // DraggableElementId : ElementId
  } with
    static member Rendered mdl draggableElementId dispatch gen =
      match mdl.Moving with
      | None ->
        Rendering.renderHandle mdl draggableElementId dispatch gen
      | Some _ ->
        // since a handle only listens for drags, render it as normal (no listeners) if there is a drag.
        gen.Render()
    static member Deferred gen =
      { Generator = gen }
    member this.Render model id dispatch =
      match model.Moving with
      | None ->
        Rendering.renderHandle model id dispatch this.Generator
      | Some _ ->
        this.Generator.Render()

  let internal renderDragHandle dragStatus model config id dispatch (handle : DragHandle) =
    match dragStatus with
    | NoActiveDrag ->
      handle.Render model id dispatch
    | ActiveDrag draggedElementId ->
      if id = draggedElementId then
        div [] [
          Rendering.renderDragged config model.Cursor id handle.Generator
          Rendering.renderHoverPreview config id handle.Generator
        ]
      else
        Rendering.renderWithHoverListener config model id dispatch handle.Generator

  /// An element the user can interact with to drag an element. Can reference a parent by Id, or itself (by Id).
  type DragHandleOld =
    /// Creates a handle that will drag an associated element Id
    /// Note that the elementId set here does not have to be the id of the handle, but can be
    /// a parent element that you want to drag
    static member dragHandle mdl draggableElementId dispatch (gen : ElementGenerator) = 
      match mdl.Moving with
      | None ->
        Rendering.renderHandle mdl draggableElementId dispatch gen
      | Some _ ->
        // since a handle only listens for drags, render it as normal if there is a drag.
        gen.Render()