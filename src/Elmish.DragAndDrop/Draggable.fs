namespace Elmish.DragAndDrop

[<AutoOpen>]
module Draggable =
  open Types
  open Fable.React
  open Fable.React.Props
  open Elmish.DragAndDrop.Helpers
  open Elmish.DragAndDrop.Helpers.HelperTypes

  module internal Rendering =
    /// Renders a handle, a collection of elements with a drag listener.
    let renderHandle mdl id dispatch gen =
      let listener = (Listeners.defaultDraggable mdl id dispatch) :> IHTMLProp
      gen
      |> ElementGenerator.addProps [listener]
      |> ElementGenerator.render

    let renderDragged cursor eleDispatch defaultClass styles props content =
      // let idProp = (Id eleDispatch.Id) :> IHTMLProp
      // let appendedStyles = [
      //   PointerEvents "none"
      //   Left cursor.x
      //   Top cursor.y
      // ]
      // let styles = (defaultList styles) @ appendedStyles
      div [] []

    let renderHoverPreview msging defaultClass styles props content =
      div [] []

  let internal renderDraggable dragStatus mdl config id dispatch (gen : ElementGenerator) =
    match dragStatus with
    | NoActiveDrag ->
      //render item as a draggable
      //Rendering.renderHandle mdl eleDispatch gen
      gen.Render()
    | ActiveDrag draggedElementId ->
      if id = draggedElementId then
        gen.Render()
      else
        gen.Render()

  type DragHandle =
    /// Creates a handle that will drag an associated element Id
    /// Note that the elementId set here does not have to be the id of the handle, but can be
    /// a parent element that you want to drag
    static member dragHandle mdl id dispatch (gen : ElementGenerator) = 
      match mdl.Moving with
      | None ->
        //let eleDis = ElementDispatch.Create id dispatch
        Rendering.renderHandle mdl id dispatch gen
      | Some _ ->
        // since a handle only listens for drags, render it as normal if there is a drag.
        gen.Render()

