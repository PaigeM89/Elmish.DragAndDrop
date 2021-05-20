namespace Elmish


module DragAndDrop =
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
    DraggableElementStyles : CSSProp list option
    DraggableElementProperties : IHTMLProp list option
    DefaultClass : string option
  } with
    static member Empty() = {
      DraggedElementStyles = None
      DraggedElementProperties = None
      HoverPreviewElementStyles = None
      HoverPreviewElementProperties = None
      SlidingElementStyles = None
      SlidingElementProperties = None
      DraggableElementStyles = None
      DraggableElementProperties = None
      DefaultClass = None
    }

  type ElementDispatch = {
    Id : ElementId
    Dispatch : (Msg -> unit)
  } with
    static member Create mdl id dis = {
      Id = id
      Dispatch = dis
    }

  type DragStatus =
  | NoActiveDrag
  | ActiveDrag of draggedElementId : ElementId

  module internal Rendering =
    /// Renders a handle, a collection of elements with a drag listener.
    let renderHandle mdl eleDispatch props content =
      let listener = (Listeners.defaultDraggable mdl eleDispatch.Id eleDispatch.Dispatch) :> IHTMLProp
      let props = listener :: props
      div props content

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

  let private render dragStatus mdl config eleDispatch props content =
    match dragStatus with
    | NoActiveDrag ->
      //render item as a draggable
      Rendering.renderHandle mdl eleDispatch props content
    | ActiveDrag draggedElementId ->
      if eleDispatch.Id = draggedElementId then
        div [] []
      else
        div [] []
      

  type DragHandle2 = {
    Styles : CSSProp list
    Props : IHTMLProp list
    Content : ReactElement list
    ClassName : string option
    HandleForElement : ElementId
  } with
    static member dragHandle2 cn h styles props content = {
      ClassName = cn
      HandleForElement = h
      Styles = styles
      Props = props
      Content = content
    }


  type DragHandle =
    /// Creates a handle that will drag an associated element Id
    /// Note that the elementId set here does not have to be the id of the handle, but can be
    /// a parent element that you want to drag
    static member dragHandle mdl eleDispatch props children = 
      match mdl.Moving with
      | None ->
        Rendering.renderHandle mdl eleDispatch props children
      | Some _ ->
        div props children

  type DropArea =
    static member dropArea model dispatch config props (handles : DragHandle2 list) =
      match model.Moving with
      | None ->
        let children =
          handles
          //|> List.map (fun (eleDispatch, handle) -> render NoActiveDrag model config eleDispatch handle )
        div [] []
      | _ -> div [] []

  module internal ItemMoving =
    open Elmish.DragAndDrop
    open Fable.Core

    let private moveItemSameList listIndex startIndex insertAtIndex li =
      match List.tryItem listIndex li with
      | Some innerList ->
        // if we're inserting an item at a later point in the list...
        if startIndex < insertAtIndex then
          // grab everything up to the start
          let beginning, rest = List.split startIndex innerList
          // grab the middle & end sections
          let middle, _end = List.split (insertAtIndex - startIndex + 1) rest
          // this middle section should be 2 items - split it & swap them
          let x, y = List.split 1 middle
          let newList = beginning @ y @ x @ _end
          List.replaceAt newList listIndex li
        // otherwise we're inserting at an earlier point in the list...
        elif startIndex > insertAtIndex then
          // grab everything up to the start
          let beginning, rest = List.split insertAtIndex innerList
          // grab the end section
          let middle, _end = List.split (startIndex - insertAtIndex) rest
          let head, tail = List.split 1 _end
          let newList = beginning @ head @ middle @ tail
          List.replaceAt newList listIndex li
        else
          li
      | None ->
        JS.console.error("Unreachable state: cannot find list at index", listIndex)
        li

    let moveItem (startListIndex, startIndex) (insertListIndex, insertAtIndex) li =
      if startListIndex = insertListIndex then
        moveItemSameList startListIndex startIndex insertAtIndex li
      else
        // lists are not the same; grab the item, insert it to the new list, and remove it from the old list
        match List.tryItem startListIndex li, List.tryItem insertListIndex li with
        | Some startList, Some insertList ->
          match List.tryItem startIndex startList with
          | Some item ->
            let newStartList = List.removeAt startIndex startList
            let newInsertList = List.insertAt item insertAtIndex insertList

            li
            |> List.replaceAt newStartList startListIndex
            |> List.replaceAt newInsertList insertListIndex
          | None ->
            JS.console.error("Unreachable state: cannot find item in list at index", startListIndex, startIndex)
            li
        | None, _ ->
          JS.console.error("Unreachable state: cannot find list at index", startListIndex)
          li
        | _, None ->
          JS.console.error("Unreachable state: cannot find list at index", insertListIndex)
          li
  
  let update msg model =
    match msg with
    | DragStart (loc, startCoords, offset) ->
      let movingStatus = MovingStatus.Init (loc) |> Some
      { model with Moving = movingStatus; Cursor = startCoords; Offset = Some offset }, Cmd.none
    | OnDrag coords ->
      {model with Cursor = coords }, Cmd.none
    | DragOver (listIndex, index, elementId) ->
      match model.Moving with
      | Some { StartLocation = (startList, startIndex, startingElementId) }->
        let slide = None //tryGetSlide elementId
        let items' =
          ItemMoving.moveItem (startList, startIndex) (listIndex, index) model.Items
          |> Model.updateItemLocations
        let mdl = { model with Items = items' } |> Model.buildItemDict // |> Model.setSlideOpt slide
        let newStartLoc = Model.getItemLocation startingElementId mdl
        match newStartLoc with
        | None ->
          mdl, Cmd.none
        | Some loc ->
          let mdl = Model.setDragSource loc mdl
          mdl, Cmd.none
      | None ->
        model, Cmd.none
    | DragEnd ->
      { model with Moving = None; Offset = None }, Cmd.none