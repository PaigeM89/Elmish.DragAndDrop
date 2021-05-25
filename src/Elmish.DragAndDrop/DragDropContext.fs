namespace Elmish

open Elmish
open Elmish.DragAndDropHelpers
open Elmish.DragAndDropHelpers.HelperTypes
open Elmish.Throttle


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

    ThrottleState: Map<string, Throttle.Status>
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
      ThrottleState = Map.empty
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

  /// Status of the current drag, if any.
  type DragStatus =
  /// No drag currently happening
  | NoActiveDrag
  /// The user is dragging an element
  | ActiveDrag of draggedElementId : ElementId

  module DragAndDropModel =
    
    /// Returns the status of the drag, if any. If there is an active drag, the element Id will be returned too.
    let toDragStatus (model : DragAndDropModel) =
      match model.Moving with
      | Some { StartLocation = (_, _, elementId) } -> ActiveDrag elementId
      | None -> NoActiveDrag

    /// Creates a new Model initialied with items in multiple lists
    let createWithItemsMultiList (items : ElementId list list) =
      let itemsWithLocs = initItemLocations items
      { DragAndDropModel.Empty() with Items = itemsWithLocs }

    /// Creates a new Model initialized with items in a single list
    let createWithItems (items : ElementId list) =
      let itemsWithLocs = initItemLocations [items]
      { DragAndDropModel.Empty() with Items = itemsWithLocs }

    /// Looks up the listindex, index of an item by Id, or None if that Id is not found.
    let findItemById (itemId : string) model =
      model.Items
      |> List.tryPick(fun li ->
        li
        |> List.tryPick (fun (listIndex, index, elementId) -> if itemId = elementId then Some (listIndex, index) else None)
      )

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
  | ThrottleMsg of Throttle.Msg

  // ************************************************************************************
  // OTHER TYPES
  // ************************************************************************************

  open Fable.React
  open Fable.React.Props

  let private getLocationForElement itemId model = 
    model.Items
    |> List.map (fun li -> li |> List.tryFind (fun (_, _, id) -> id = itemId))
    |> List.choose id
    |> List.tryHead

  module internal Listeners =
    open Browser.Types
    open Elmish.DragAndDropHelpers.BrowserHelpers
    open Fable.Core

    let defaultDraggable model (draggableId : DraggableId) dispatch =
      let loc = getLocationForElement draggableId model
      match loc with
      | Some loc ->
        OnMouseDown (fun (ev : Browser.Types.MouseEvent) ->
          ev.preventDefault()
          let o = getOffset ev (locId loc)
          (loc, fromME ev, o) |> DragStart |> dispatch
        )
      | None -> 
        JS.console.error(sprintf "No location found for element with id '%s' in drag and drop items" draggableId)
        OnMouseDown (fun (ev : Browser.Types.MouseEvent) -> ())

    let defaultMouseMoveListener dispatch =
      OnMouseMove (fun (ev : MouseEvent) ->
        ev.preventDefault()
        let c = coords ev.clientX ev.clientY
        DragAndDropMsg.OnDrag c |> dispatch
      )

    /// Listener for when another element is being dragged and is moved over this element.
    let defaultHoverListener model id dispatch throttleTimeSpan =
      OnMouseEnter (fun (ev : MouseEvent) ->
        ev.preventDefault()
        match throttleTimeSpan with
        | Some timespan ->
            let isThrottled = throttle model.ThrottleState id timespan ev
            match isThrottled with
            | None -> ()
            | Some (ev, throttleMsg) ->
                throttleMsg |> ThrottleMsg |> dispatch
                let loc = getLocationForElement id model
                match loc with
                | Some loc ->
                    DragOver loc |> dispatch
                | None -> ()
        | None ->
            let loc = getLocationForElement id model
            match loc with
            | Some loc ->
                DragOver loc |> dispatch
            | None -> ()
      )

    let defaultReleaseListener dispatch =
      OnMouseUp (fun (ev : MouseEvent) -> ev.preventDefault(); DragEnd |> dispatch)

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

    MoveThrottleTimeMs : System.TimeSpan option
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
      MoveThrottleTimeMs = None
    }

  /// Used to generate a `ReactElement` after applying any appropriate styles or properties.
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

  module internal Building =
    let private orEmpty li = Option.defaultValue [] li
    // because property ordering is important, this reverses the append on a list
    let private appendR li1 li2 = li2 @ li1
    
    let buildDragged config cursor id gen =
      let idProp = (Id id) :> IHTMLProp
      let appendedStyles = 
        config.DraggedElementStyles |> orEmpty |> appendR [
          PointerEvents "none"
          Left cursor.x
          Top cursor.y
        ]
      let props = config.DraggedElementProperties |> orEmpty |> appendR [idProp]
      gen
      |> ElementGenerator.addProps props
      |> ElementGenerator.addStyles appendedStyles

    let buildHoverPreview config id gen =
      let idProp = (Id id) :> IHTMLProp
      let styles = config.HoverPreviewElementStyles |> orEmpty
      let props = config.HoverPreviewElementProperties |> orEmpty |> appendR [ idProp ]
      gen
      |> ElementGenerator.addStyles styles
      |> ElementGenerator.addProps props

    let buildHoverListener config model id dispatch gen =
      let listener = Listeners.defaultHoverListener model id dispatch config.MoveThrottleTimeMs
      let styles = config.ListenerElementStyles |> orEmpty
      let props = config.ListenerElementProperties |> orEmpty |> appendR [listener]
      gen
      |> ElementGenerator.addStyles styles
      |> ElementGenerator.addProps props

  module internal Rendering =
    let private orEmpty li = Option.defaultValue [] li
    // because property ordering is important, this reverses the append on a list
    let private appendR li1 li2 = li2 @ li1

    /// Renders a handle, a collection of elements with a drag listener.
    let renderHandle mdl (id : DraggableId) dispatch gen =
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
      let listener = Listeners.defaultHoverListener model id dispatch config.MoveThrottleTimeMs
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

  /// Defines an object that can be clicked on to drag elements.
  type DragHandle =
    /// Creates a new `DragHandle`. The `id` passed here is the id of the `Draggable`, NOT the id of this
    /// `DragHandle`. The `DragHandle` id should be defined in the `gen` given to this function.
    static member dragHandle model (id : DraggableId) dispatch gen =
      match model.Moving with
      | None ->
        Rendering.renderHandle model id dispatch gen
      | Some _ ->
        // since a handle only listens for drags, render it as normal (no listeners) if there is a drag.
        gen.Render()

  /// Defines an object that can be dragged. A `Draggable` is not automatically able to be 
  /// dragged unless it somehow includes a `DragHandle`; a `Draggable` is merely a way to identify
  /// elements that should be able to move.
  type Draggable = {
    Generators : ElementGenerator list
  } with
    /// Creates a new `Draggable`. The `ElementGenerator` passed in must contain a `DragHandle` somewhere in order for
    /// this element to be able to be dragged.
    static member draggable model config dispatch gen = 
      match model.Moving with
      | None ->
        { Generators = [gen] }
      | Some { StartLocation = (_, _, draggedElementId )} ->
        if gen.Id = draggedElementId then
          let draggedGenerator = Building.buildDragged config model.Cursor gen.Id gen
          let hoverPreviewGenerator = Building.buildHoverPreview config gen.Id gen
          { Generators = [hoverPreviewGenerator; draggedGenerator] }
        else
          { Generators = [ Building.buildHoverListener config model gen.Id dispatch gen ] }

    /// Creates a new `Draggable` where the whole element is a `DragHandle`. This will cause all of the children
    /// to not be able to be clicked on, so interactable elements won't work as expected.
    static member asDragHandle model config dispatch gen =
      let handleId = gen.Id + "-handle"
      let handleGen = ElementGenerator.Create handleId [] [] gen.Content
      match model.Moving with
      | None ->
        let handle = DragHandle.dragHandle model gen.Id dispatch handleGen
        let gen = { gen with Content = [ handle ]}
        { Generators = [ gen ]}
      | Some { StartLocation = (_, _, draggedElementId )} ->
        let handle = DragHandle.dragHandle model gen.Id dispatch handleGen
        let gen = { gen with Content = [ handle ]}
        if gen.Id = draggedElementId then
          let draggedGenerator = Building.buildDragged config model.Cursor gen.Id gen
          let hoverPreviewGenerator = Building.buildHoverPreview config gen.Id gen
          { Generators = [hoverPreviewGenerator; draggedGenerator] }
        else
          { Generators = [ Building.buildHoverListener config model gen.Id dispatch gen ] }

  // ************************************************************************************
  // DROP AREA
  // ************************************************************************************

  /// An area where `Draggables` live and can be moved.
  type DropArea =

    /// Creates a `DropArea` from the given `Draggables`.
    static member fromDraggables tag props (draggables : Draggable list) =
      let content = draggables |> List.map (fun x -> x.Generators |> List.map (fun g -> g.Render())) |> List.concat
      tag props content

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
      { model with Moving = movingStatus; Cursor = startCoords; Offset = Some offset }, Cmd.none
    | DragAndDropMsg.OnDrag coords ->
      {model with Cursor = coords }, Cmd.none
    | DragOver (listIndex, index, elementId) ->
      match model.Moving with
      | Some { StartLocation = (startList, startIndex, startingElementId) }->
        let slide = None //tryGetSlide elementId
        let items' =
          ItemMoving.moveItem (startList, startIndex) (listIndex, index) model.Items
          |> getUpdatedItemLocations
        let mdl = { model with Items = items' } // |> Model.setSlideOpt slide
        let newStartLoc = (listIndex, index, startingElementId)
        (setDragSource newStartLoc mdl), Cmd.none
      | None ->
        model, Cmd.none
    | DragEnd ->
      { model with Moving = None; Offset = None }, Cmd.none
    | ThrottleMsg throttleMsg ->
      // let the throttler handle the message
      let throttleResult = handleThrottleMsg throttleMsg model.ThrottleState
      match throttleResult with
      // get back a new state and a command 
      | Ok (throttleState, throttleCmd) ->
          // map the command so it's run
          { model with ThrottleState = throttleState }, Cmd.map ThrottleMsg throttleCmd
      | Error e ->
          //printfn "Error throttling: %A" e
          Fable.Core.JS.console.error("Error throttling: ", e)
          model, Cmd.none
