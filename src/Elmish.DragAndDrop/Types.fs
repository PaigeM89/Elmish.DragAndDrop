namespace Elmish.DragAndDrop

[<AutoOpen>]
module Types =
  open Elmish
  open Fable.React
  open Fable.React.Props
  open Elmish.DragAndDrop.Helpers
  open Elmish.DragAndDrop.Helpers.HelperTypes

  type Model = {
    /// The cursor's current coordinates, updated when dragging to draw the ghost.
    Cursor : Coords
    /// The index of the currently moving item (or, rather, the point it is hovering over), and that element's id
    /// The slide contains the starting coordinates & element that is sliding.
    Moving : MovingStatus option
    /// A lookup to know exactly where each element is located
    LocationDict : Map<ElementId, ItemLocation>
    /// The amount to adjust the drag ghost so that it is correctly placed under the cursor.
    Offset : Coords option
    /// The items to be sorted. If there are multiple containers, use multiple lists.
    Items : (ItemLocation) list list
  } with
    /// Returns the Ids of each component in the model, in the order they are currently sorted
    member this.ElementIds() =
      this.Items
      |> List.map (fun itemList ->
        itemList |> List.map (fun (_, _, id) -> string id)
      )

    /// Returns a new instance of an empty Drag And Drop model
    static member Empty() = {
      Cursor = { x = 0.; y = 0. }
      Moving = None
      LocationDict = Map.empty
      Offset = None
      Items = []
    }

  let private initItemLocations lists =
    lists
    |> List.mapi(fun i li ->
      li
      |> List.mapi (fun j x ->
        (i, j, x)
      )
    )

  let private getLocationForElement elementId model = 
    model.Items
    |> List.map (fun li -> li |> List.tryFind (fun (_, _, id) -> id = elementId))
    |> List.choose id
    |> List.tryHead

  module Model =

    let buildItemDict (model : Model) =
      let d = 
        model.Items
        |> List.map(fun li ->
          li 
          |> List.map (fun (listInd, ind, id) ->
            id, (listInd, ind, id)
          )
        )
        |> List.concat
        |> Map.ofList
      { model with LocationDict = d }

    let getItemLocation id model = Map.tryFind id model.LocationDict

    /// Creates a new Model initialied with items in multiple lists
    let createWithItemsMultiList (items : ElementId list list) =
      let itemsWithLocs = initItemLocations items
      { Model.Empty() with Items = itemsWithLocs } |> buildItemDict

    /// Creates a new Model initialized with items in a single list
    let createWithItems (items : ElementId list) =
      let itemsWithLocs = initItemLocations [items]
      { Model.Empty() with Items = itemsWithLocs } |> buildItemDict

    let updateItemLocations (items : ItemLocation list list) =
      items
      |> List.mapi (fun i itemList ->
        itemList
        |> List.mapi (fun j (_, _, id) ->
          (i, j, id)
        )
      )

    let setDragSource loc model =
      match model.Moving with
      | Some { Slide = Some slide} ->
        let moving = {MovingStatus.Init loc  with Slide = Some slide }
        { model with Moving = Some moving }
      | Some { Slide = None } ->
        let moving = MovingStatus.Init loc
        { model with Moving = Some moving }
      | None ->
        let moving = MovingStatus.Init loc
        { model with Moving = Some moving }

  type Msg =
  /// Indicates dragging has started on an element. Requires the item location and starting coordinates.
  | DragStart of loc : ItemLocation * start : Coords * offset : Coords
  /// An item is currently being dragged. This updates the location of the cursor.
  | OnDrag of coords : Coords
  /// An item is currently being dragged and just moved over another item in the list.
  | DragOver of loc : ItemLocation
  /// The item was released and the drag has ended.
  | DragEnd

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
    DraggedElementStyles : CSSProp list option
    DraggedElementProperties : IHTMLProp list option
    HoverPreviewElementStyles : CSSProp list option
    HoverPreviewElementProperties : IHTMLProp list option
    SlidingElementStyles : CSSProp list option
    SlidingElementProperties : IHTMLProp list option
    // DraggableElementStyles : CSSProp list option
    // DraggableElementProperties : IHTMLProp list option
    ListenerElementStyles : CSSProp list option
    ListenerElementProperties : IHTMLProp list option
  } with
    static member Empty() = {
      DraggedElementStyles = None
      DraggedElementProperties = None
      HoverPreviewElementStyles = None
      HoverPreviewElementProperties = None
      SlidingElementStyles = None
      SlidingElementProperties = None
      // DraggableElementStyles = None
      // DraggableElementProperties = None
      ListenerElementStyles = None
      ListenerElementProperties = None
    }

  type ElementDispatch = {
    Id : ElementId
    Dispatch : (Msg -> unit)
  } with
    static member Create id dis = {
      Id = id
      Dispatch = dis
    }

  type DragStatus =
  | NoActiveDrag
  | ActiveDrag of draggedElementId : ElementId

  type ElementGenerator = {
    Id : ElementId
    Styles : CSSProp list
    Props : IHTMLProp list
    Content : ReactElement list
  } with
    static member Create id styles props content : ElementGenerator =
      { Id = id; Styles = styles; Props = props; Content = content }
    member this.AddStyles newStyles = { this with Styles = this.Styles @ newStyles }
    member this.AddProps newProps = { this with Props = this.Props @ newProps }
    member this.AddContent newContent = { this with Content = this.Content @ newContent }
    member this.Render() =
      let styleProp = Style this.Styles :> IHTMLProp
      let idProp = (Id this.Id) :> IHTMLProp
      div (this.Props @ [idProp; styleProp]) this.Content

  module ElementGenerator = 
    let createGenerator id styles props content : ElementGenerator =
      { Id = id; Styles = styles; Props = props; Content = content }
    let addStyles newStyles (gen : ElementGenerator) = gen.AddStyles newStyles
    let addProps newProps (gen : ElementGenerator) = gen.AddProps newProps
    let addContent newContent (gen : ElementGenerator) = gen.AddContent newContent
    let render (gen : ElementGenerator) = gen.Render()
    let renderWith styles props gen =
      gen
      |> addStyles styles
      |> addProps props
      |> render

