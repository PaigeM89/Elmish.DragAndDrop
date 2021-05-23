namespace Elmish.DragAndDrop

[<AutoOpen>]
module Types =
  open Elmish
  open Fable.React
  open Fable.React.Props
  open Elmish.DragAndDrop.Helpers
  open Elmish.DragAndDrop.Helpers.HelperTypes
  open Model


  let private getLocationForElement elementId model = 
    model.Items
    |> List.map (fun li -> li |> List.tryFind (fun (_, _, id) -> id = elementId))
    |> List.choose id
    |> List.tryHead

  module internal Listeners =
    open Browser.Types
    open Elmish.DragAndDrop.Helpers.BrowserHelpers
    open Fable.Core

    let defaultDraggable model elementId dispatch =
      let loc = getLocationForElement elementId model
      match loc with
      | Some loc ->
        OnMouseDown (fun (ev : Browser.Types.MouseEvent) ->
          ev.preventDefault()
          let o = getOffset ev (locId loc)
          (loc, fromME ev, o) |> DragStart |> dispatch
        )
      | None -> 
        JS.console.error(sprintf "No location found for element with id '%s' in drag and drop items" elementId)
        OnMouseDown (fun (ev : Browser.Types.MouseEvent) -> ())

    let defaultMouseMoveListener dispatch =
      OnMouseMove (fun (ev : MouseEvent) ->
        ev.preventDefault()
        let c = coords ev.clientX ev.clientY
        OnDrag c |> dispatch
      )

    /// Listener for when another element is being dragged and is moved over this element.
    let defaultHoverListener model id dispatch =
      OnMouseEnter (fun (ev : MouseEvent) ->
        ev.preventDefault()
        let loc = getLocationForElement id model
        match loc with
        | Some loc ->
          DragOver loc |> dispatch
        | None -> ()
      )

    let defaultReleaseListener dispatch =
      OnMouseUp (fun (ev : MouseEvent) -> ev.preventDefault();  DragEnd |> dispatch)

  type DragAndDropConfig = {
    /// CSS Styles applied to the currently dragged element.
    DraggedElementStyles : CSSProp list option
    /// HTML Properties applied to the currently dragged element.
    DraggedElementProperties : IHTMLProp list option
    /// CSS Styles applied to the hover preview element.
    HoverPreviewElementStyles : CSSProp list option
    /// HTML Properties applied to the hover preview element.
    HoverPreviewElementProperties : IHTMLProp list option
    /// CSS Styles applied to the sliding element, if any. Not currently implemented.
    SlidingElementStyles : CSSProp list option
    /// HTML Properties applied to the sliding element, if any. Not currently implemented.
    SlidingElementProperties : IHTMLProp list option
    /// CSS Styles applied to the elements listening for a hover event.
    /// During a drag, this is all elements.
    ListenerElementStyles : CSSProp list option
    /// HTML Properties applied to the elements listening for a hover event.
    /// During a drag, this is all elements.
    ListenerElementProperties : IHTMLProp list option
  } with
    static member Empty() = {
      DraggedElementStyles = None
      DraggedElementProperties = None
      HoverPreviewElementStyles = None
      HoverPreviewElementProperties = None
      SlidingElementStyles = None
      SlidingElementProperties = None
      ListenerElementStyles = None
      ListenerElementProperties = None
    }

  /// Status of the current drag, if any.
  type DragStatus =
  /// No drag currently happening
  | NoActiveDrag
  /// The user is dragging an element
  | ActiveDrag of draggedElementId : ElementId

  /// Used to generate a `ReactElement` after applying any appropriate styles.
  type ElementGenerator = {
    /// The Id of the element to generate. This will be automatically added to the Props when
    /// the ReactElement is generated.
    Id : ElementId
    Tag : IHTMLProp seq -> ReactElement seq -> ReactElement
    /// The default CSS Styles of the element to generate
    Styles : CSSProp list
    /// The default HTML Properties of the element to generate
    Props : IHTMLProp list
    /// The content of the element to generate.
    Content : ReactElement list
  } with
    static member Create id styles props content : ElementGenerator =
      { Id = id; Tag = div; Styles = styles; Props = props; Content = content }
    static member FromSingleElement id styles props content = 
      ElementGenerator.Create id styles props [content]
    member this.AddStyles newStyles = { this with Styles = this.Styles @ newStyles }
    member this.AddProps newProps = { this with Props = this.Props @ newProps }
    member this.AddContent newContent = { this with Content = this.Content @ newContent }
    member this.SetTag tag = { this with Tag = tag }
    /// Turns this Generator into a `ReactElement`. The Id Property of this record is turned into
    /// an HTML Property and added to the properties.
    member this.Render() =
      let styleProp = Style this.Styles :> IHTMLProp
      let idProp = (Id this.Id) :> IHTMLProp
      let props = this.Props @ [idProp; styleProp] |> Seq.ofList
      this.Tag props (Seq.ofList this.Content)

  module ElementGenerator = 
    let createGenerator id styles props content : ElementGenerator =
      { Id = id; Tag = div; Styles = styles; Props = props; Content = content }
    let addStyles newStyles (gen : ElementGenerator) = gen.AddStyles newStyles
    let addProps newProps (gen : ElementGenerator) = gen.AddProps newProps
    let addContent newContent (gen : ElementGenerator) = gen.AddContent newContent
    let setTag tag (gen : ElementGenerator) = gen.SetTag tag
    let render (gen : ElementGenerator) = gen.Render()
    let renderWith styles props gen =
      gen
      |> addStyles styles
      |> addProps props
      |> render

