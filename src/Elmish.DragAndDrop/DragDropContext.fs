namespace Elmish

open Elmish.DragAndDropHelpers
open Elmish.DragAndDropHelpers.HelperTypes

module DragAndDrop =

  // ************************************************************************************
  // MODEL
  // ************************************************************************************

  type DragAndDropModel = {
    /// The cursor's current coordinates, updated when dragging to draw the ghost.
    Cursor : Coords
    /// The index of the currently moving item (or, rather, the point it is hovering over), and that element's id.
    /// The slide contains the starting coordinates & element that is sliding.
    Moving : MovingStatus option
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

  /// Returns a list of updated Item Locations from the given list
  let internal getUpdatedItemLocations (items : ItemLocation list list) =
    items
    |> List.mapi (fun i itemList ->
      itemList
      |> List.mapi (fun j (_, _, id) ->
        (i, j, id)
      )
    )

  module DragAndDropModel =

    /// Creates a new Model initialied with items in multiple lists
    let createWithItemsMultiList (items : ElementId list list) =
      let itemsWithLocs = initItemLocations items
      { DragAndDropModel.Empty() with Items = itemsWithLocs }

    /// Creates a new Model initialized with items in a single list
    let createWithItems (items : ElementId list) =
      let itemsWithLocs = initItemLocations [items]
      { DragAndDropModel.Empty() with Items = itemsWithLocs }

    /// Inserts a new item at the specified indexes. If a list does not exist at the list index, 
    /// a new list will be created at the last index.
    let insertNewItemAt listIndex itemIndex (itemId : string) model =
      let loc = (listIndex, itemIndex, itemId)
      let lio = List.tryItem listIndex model.Items
      match lio with
      | Some li ->
        let li = List.insertAt loc itemIndex li
        let lis = List.replaceAt li listIndex model.Items
        let allItemLocs = getUpdatedItemLocations lis
        { model with Items = allItemLocs }
      | None ->
        let newListIndex = model.Items.Length
        let li = [(newListIndex, 0, itemId)]
        let lis = model.Items @ [li]
        let allItemLocs = getUpdatedItemLocations lis
        { model with Items = allItemLocs }
    
    /// Inserts a new item at the head of the given list index. If there is no list at that list index,
    /// a new list will be created at the last list index, with this item as the only item.
    let insertNewItemAtHead listIndex item model = insertNewItemAt listIndex 0 item model
    
    /// Removes the item at the given list index, item index. If either index is out of range, nothing happens.
    let removeItemAt listIndex itemIndex model =
      match List.tryItem listIndex model.Items with
      | Some li ->
        let li = List.removeAt itemIndex li
        let lis = List.replaceAt li listIndex model.Items
        let allItemLocs = getUpdatedItemLocations lis
        { model with Items = allItemLocs }
      | None -> model

    /// Removes the item by id. If the id is not found, nothing happens.
    let removeItem itemId model =
      let picked = 
        model.Items
        |> List.concat
        |> List.tryFind(fun (_, _, item) -> item = itemId)
      match picked with
      | Some (listIndex, index, _) ->
        removeItemAt listIndex index model
      | None -> model

    /// Replaces the item at the given list index, item index. If the previous item is not found, the item is simply inserted.
    /// If the list does not exist at the given list index, a new list is created with this item as the only item.
    let replaceItemAt listIndex itemIndex newItem model =
      insertNewItemAt listIndex itemIndex newItem model
      |> removeItemAt listIndex (itemIndex + 1)


  let internal setDragSource loc model =
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

  type DragAndDropMsg =
  /// Indicates dragging has started on an element. Requires the item location and starting coordinates.
  | DragStart of loc : ItemLocation * start : Coords * offset : Coords
  /// An item is currently being dragged. This updates the location of the cursor.
  | OnDrag of coords : Coords
  /// An item is currently being dragged and just moved over another item in the list.
  | DragOver of loc : ItemLocation
  /// The item was released and the drag has ended.
  | DragEnd

  // ************************************************************************************
  // OTHER TYPES
  // ************************************************************************************

  open Fable.React
  open Fable.React.Props

  let private getLocationForElement elementId model = 
    model.Items
    |> List.map (fun li -> li |> List.tryFind (fun (_, _, id) -> id = elementId))
    |> List.choose id
    |> List.tryHead

  module internal Listeners =
    open Browser.Types
    open Elmish.DragAndDropHelpers.BrowserHelpers
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
        DragAndDropMsg.OnDrag c |> dispatch
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

  // ************************************************************************************
  // DRAG HANDLE
  // ************************************************************************************

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
        gen.Tag (Seq.empty) (Seq.ofList [
          Rendering.renderDragged config model.Cursor id gen
          Rendering.renderHoverPreview config id gen
        ])
      else
        Rendering.renderWithHoverListener config model id dispatch gen

  let internal toDraggables dragStatus model config id dispatch (gen : ElementGenerator) =
    match dragStatus with
    | NoActiveDrag ->
      //render item as a draggable
      //a Generator should already have a handle defined in it (or is a handle itself).
      [gen.Render()]
    | ActiveDrag draggedElementId ->
      if id = draggedElementId then
        [
          Rendering.renderDragged config model.Cursor id gen
          Rendering.renderHoverPreview config id gen
        ]
      else
        [Rendering.renderWithHoverListener config model id dispatch gen]

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

  // ************************************************************************************
  // DROP AREA
  // ************************************************************************************


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
        let children =
          content
          |> List.map (fun (elementId, handle) ->
            renderDragHandle (DragStatus.ActiveDrag draggedElementId) model config elementId dispatch handle
          )
        div props children

  // ************************************************************************************
  // DRAG AND DROP CONTEXT
  // ************************************************************************************

  type DragDropContext =
    static member context model dispatch tag (props : IHTMLProp list) content =
      match model.Moving with
      | None ->
        tag props content
      | Some _ ->
        let props = [
          yield! props
          Listeners.defaultReleaseListener dispatch
          Listeners.defaultMouseMoveListener dispatch
        ]
        tag props content

  // ************************************************************************************
  // UPDATE
  // ************************************************************************************


  module internal ItemMoving =
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
  
  let dragAndDropUpdate msg model =
    match msg with
    | DragStart (loc, startCoords, offset) ->
      let movingStatus = MovingStatus.Init (loc) |> Some
      { model with Moving = movingStatus; Cursor = startCoords; Offset = Some offset }
    | DragAndDropMsg.OnDrag coords ->
      {model with Cursor = coords }
    | DragOver (listIndex, index, elementId) ->
      match model.Moving with
      | Some { StartLocation = (startList, startIndex, startingElementId) }->
        let slide = None //tryGetSlide elementId
        let items' =
          ItemMoving.moveItem (startList, startIndex) (listIndex, index) model.Items
          |> getUpdatedItemLocations
        let mdl = { model with Items = items' } // |> Model.setSlideOpt slide
        let newStartLoc = (listIndex, index, startingElementId)
        ( setDragSource newStartLoc mdl)
      | None ->
        model
    | DragEnd ->
      { model with Moving = None; Offset = None }

  let updateWithCmd msg model =
    let mdl = dragAndDropUpdate msg model
    mdl, Cmd.none
