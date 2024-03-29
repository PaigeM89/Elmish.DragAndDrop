namespace Elmish.DragAndDrop

open System
open Elmish
open Elmish.DragAndDropHelpers
open Elmish.DragAndDropHelpers.HelperTypes
open Elmish.Throttle
open Fable.React
open Fable.React.Props

[<AutoOpen>]
module Types =
  type Tag = IHTMLProp seq -> ReactElement seq -> ReactElement
  type StyledTag = CSSProp seq -> Tag
  type Props = IHTMLProp seq

  type ElementIndex = Index * ElementId
  type ListIndexTree = Map<ListIndex, ElementIndex list>

  let addIndexesToList (li : ElementId list) = List.mapi (fun i id -> i, id) li

  /// Turns a list of lists into an indexed map of lists
  let groupAndMapLists (lists : ElementIndex list list) =
    let indexed =
      lists
      |> List.mapi (fun index li -> index, li)
    indexed |> Map.ofList

  type ItemTree = Map<DragAndDropCategoryKey, Map<ListIndex, (Index * ElementId) list>>

  let private initItemLocationsForTree lists =
    lists
    |> List.mapi (fun i li ->
      i, (li |> List.mapi (fun j x -> j, x))
    )
    |> Map.ofList

  type DragAndDropModel = {
    Cursor : Coords
    Moving: MovingStatus option
    Offset : Coords option
    ItemTree : ItemTree
    ThrottleState : Map<string, Throttle.Status>
  } with
      /// returns TRUE if there is a current drag in progress.
      member this.IsDragging() = this.Moving.IsSome

      /// Returns the element Ids in this drag & drop across ALL categories,
      /// aggregated by list index. You probably want to use `ElementIdsForCategory` instead,
      /// unless you only have a single category.
      member this.ElementIds() =
        this.ItemTree
        |> Map.toList
        |> List.collect (fun (_, m) -> 
          m
          |> Map.toList
          |> List.map (fun (listIndex, li) -> listIndex, (li |> List.map snd))
        )

      /// Returns the elementIds in a single list across ALL categories. You probably want to 
      /// use `ElementIdsForCategorySingleList` instead, unless you only have a single category.
      [<Obsolete("Use `ElementIdsForCategory` instead.")>]
      member this.ElementIdsSingleList() =
        this.ItemTree
        |> Map.toList
        |> List.collect (fun (key, m) -> m |> Map.toList |> List.collect (fun (_, li) -> li |> List.map snd))

      /// Returns the list of list index, lists of (index, elementid) for the given category key.
      member this.ElementIdsForCategory key =
        this.ItemTree
        |> Map.tryFind key
        |> Option.map Map.toList
        |> Option.defaultValue []

      /// Returns the elementIds for the given category key aggregated into a single list.
      member this.ElementIdsForCategorySingleList key =
        this.ElementIdsForCategory key
        |> List.collect (fun (_, li) -> li |> List.map snd )

      static member Empty<'a>() = {
        Cursor = { x = 0.; y = 0. }
        Moving = None
        Offset = None
        ItemTree = Map.empty
        ThrottleState = Map.empty
      }

      /// Removes all drag and drop info for the given key. Useful if a drag-and-drop is no longer on the screen.
      member this.RemoveKey key = { this with ItemTree = this.ItemTree |> Map.remove key }
      
      /// Adds or replaces all the elementIds for the given CategoryKey.
      member this.AddOrReplaceKey key elementIds =
        let listIndex = initItemLocationsForTree elementIds
        { this with ItemTree = this.ItemTree |> Map.add key listIndex }


  let private initItemLocations lists =
    lists
    |> List.mapi(fun i li ->
      li
      |> List.mapi (fun j x ->
        (i, j, x)
      )
    )

  let internal denseRankElementIndexes (lit : ListIndexTree) =
    lit
    |> Map.toList
    |> List.map(fun (k, listIndexes) ->
      let listIndexes = listIndexes |> List.mapi (fun i (_, id) -> i, id)
      k, listIndexes 
    )
    |> Map.ofList

  let internal replaceItemsLocationsAtKey model key items =
    let i = model.ItemTree |> Map.add key items
    { model with ItemTree = i }

  /// Status of the current drag, if any.
  type DragStatus =
  /// No drag currently happening
  | NoActiveDrag
  /// The user is dragging an element
  | ActiveDrag of category : DragAndDropCategoryKey * draggedElementId : ElementId

  module DragAndDropModel =
    type InitialIds = string list list

    let getItemsForCategoryOrEmpty key model =
      model.ItemTree |> Map.tryFind key |> Option.defaultValue Map.empty

    /// Returns the status of the drag, if any. If there is an active drag, the element Id will be returned too.
    let toDragStatus (model : DragAndDropModel) =
      match model.Moving with
      | Some { StartLocation = (key, _, _, elementId) } -> ActiveDrag (key, elementId)
      | None -> NoActiveDrag

    /// Creates an empty Drag & Drop model with the pre-set categories in the list
    /// Note that this does not later prevent categories from being added or dropped.
    let createWithCategories (keys : DragAndDropCategoryKey list) =
      let itemMap = keys |> List.map (fun k -> k, Map.empty) |> Map.ofList
      { DragAndDropModel.Empty() with ItemTree = itemMap }

    /// Creates a Drag & Drop model with the given categories, where each 
    /// category is associated with a list of list of element Ids. If the drag & drop category is intended
    /// to be used as a single list (not a multi-drop-area list), then simply supply
    /// a list of element Ids as the only element in a list.
    /// 
    /// Example: 
    /// ```[
    ///   ("categoryKey", [ [ "elementOne"; "elementTwo" ] ]); 
    ///   ("otherCategoryKey", [ [ "elementThree" ]; [ "elementFour" ] ] )
    ///  ]
    /// ```
    let createWithCategoriesAndItems (values : (DragAndDropCategoryKey * InitialIds) list ) =
      let itemMap  = 
        values
        |> List.map (fun (category, initialIds) ->
          let mappedIds =
            initialIds
            |> List.mapi (fun i x -> i, x |> List.mapi (fun j s -> j, s))
            |> Map.ofList
          category, mappedIds
        )
        |> Map.ofList
      { DragAndDropModel.Empty() with ItemTree = itemMap }

    /// Creates a new Model initialied with items in multiple lists using the default category.
    /// This is useful if you do not plan on having multiple categories.
    let createWithItemsMultiList (items : ElementId list list) =
      let itemMap =
        let mappedLists = 
          items
          |> List.map addIndexesToList
          |> groupAndMapLists
        [(DefaultCategory, mappedLists)] |> Map.ofList
      { DragAndDropModel.Empty() with ItemTree = itemMap }

    /// Creates a new Model initialized with items in a single list.
    /// This is useful if you do not plan on having multiple categories.
    let createWithItems (items : ElementId list) =
      let itemsWithLocs = initItemLocationsForTree [items]
      let itemMap = [HelperTypes.DefaultCategory, itemsWithLocs] |> Map.ofList
      { DragAndDropModel.Empty() with ItemTree = itemMap }

    /// Inserts a new item at the specified indexes. If a list does not exist at the list index, 
    /// a new list will be created at the last index.
    let insertNewItemAt key listIndex itemIndex (itemId : string) model =
      let listIndexMap = model.ItemTree |> Map.tryFind key
      match listIndexMap with
      | Some listIndexMap ->
        let li = listIndexMap |> Map.tryFind listIndex
        match li with
        | Some li ->
          let head = List.tryTake (itemIndex - 1) li
          let tail = List.skip itemIndex li
          let elements = head @ ((itemIndex, itemId) :: tail)
          let indexMap = Map.add listIndex elements listIndexMap
          let m = model.ItemTree |> Map.add key indexMap
          { model with ItemTree = m}
        | None ->
          // no list index found, create one
          let listIndexMap = [ listIndex, [(0, itemId)]] |> Map.ofList
          let m = model.ItemTree |> Map.add key listIndexMap
          { model with ItemTree = m }
      | None ->
        let indexMap = [listIndex, ([itemIndex, itemId])] |> Map.ofList
        let items = model.ItemTree |> Map.add key indexMap
        { model with ItemTree = items }

    /// Inserts a new item at the head of the given list index. If there is no list at that list index,
    /// a new list will be created at the last list index, with this item as the only item.
    let insertNewItemAtHead key listIndex item model = insertNewItemAt key listIndex 0 item model
    
    /// Removes the item at the given list index, item index. If either index is out of range, nothing happens.
    let removeItemAt key listIndex itemIndex model =
      let items = getItemsForCategoryOrEmpty key model
      match Map.tryFind listIndex items with
      | Some elements ->
        let li = List.removeAt itemIndex elements
        let indexMap = Map.add listIndex li items
        let items = model.ItemTree |> Map.add key indexMap
        { model with ItemTree = items }
      | None ->
        model

    /// Removes the item by id. If the id is not found, nothing happens.
    let removeItem key itemId model =
      let items =
        model.ItemTree
        |> Map.tryFind key
        |> Option.defaultValue Map.empty
        |> Map.toList
        |> List.map (fun (listIndex, elements) ->
          let elements = elements |> List.filter (fun (index, id) -> id <> itemId)
          listIndex, elements
        )
        |> Map.ofList
      let m = Map.add key items model.ItemTree
      { model with ItemTree = m }

    /// Replaces the item at the given list index, item index. If the previous item is not found, the item is simply inserted.
    /// If the list does not exist at the given list index, a new list is created with this item as the only item.
    let replaceItemAt key listIndex itemIndex newItem model =
      insertNewItemAt key listIndex itemIndex newItem model
      |> removeItemAt key listIndex (itemIndex + 1)

    /// Replaces all items for a given category with the given item map.
    let replaceItemsForCategory categoryKey model items =
      let it = model.ItemTree |> Map.add categoryKey items
      { model with ItemTree = it }

  let internal setDragSource key loc model =
    match model.Moving with
    | Some { Slide = Some slide} ->
      let moving = { MovingStatus.Init key loc with Slide = Some slide }
      { model with Moving = Some moving }
    | Some { Slide = None } ->
      let moving = MovingStatus.Init key loc
      { model with Moving = Some moving }
    | None ->
      let moving = MovingStatus.Init key loc
      { model with Moving = Some moving }

  type DragAndDropMsg =
  /// Indicates dragging has started on an element. Requires the item location and starting coordinates.
  /// The starting drop area id is an option because it's not always easy to have that when a drag has started.
  | DragStart of loc : ItemLocation * start : Coords * offset : Coords
  /// An item is currently being dragged. This updates the location of the cursor.
  | OnDrag of coords : Coords
  /// An item is currently being dragged and just moved over another item in the list.
  | DragOver of loc : ItemLocation
  /// An item is currently being dragged over some other item. The List Index is included as an option
  /// in case that data is available and relevant. This message is dispatched when an item enters a new
  /// Drop Area, but has not hovered over a specific item in that drop area.
  | DragOverNonDraggable of key : DragAndDropCategoryKey * elementId : string * listIndex : ListIndex option
  /// The item was released and the drag has ended.
  | DragEnd
  | ThrottleMsg of Throttle.Msg

  // ************************************************************************************
  // OTHER TYPES
  // ************************************************************************************

  let internal getLocationForElement key itemId model =
    match Map.tryFind key model.ItemTree with
    | Some indexed ->
      indexed
      |> Map.toList
      |> List.choose (fun (listIndex, elements) -> 
        match List.tryFind (fun (index, elementId) -> elementId = itemId) elements with
        | Some (index, elementId) ->
          (key, listIndex, index, elementId) |> Some
        | None -> None
      )
      |> List.tryHead
    | None -> None

  /// The Id of the throttle
  type MouseEventWithThrottle = Browser.Types.MouseEvent -> Throttle.Id -> unit

  type Placeholder = {
    Styles : CSSProp list
    Props : IHTMLProp list
    Content : ReactElement list
  }

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
    /// During a drag, this is all elements that may shift to adjust for a draggable item to be dropped.
    ListenerElementStyles : CSSProp list option
    /// HTML Properties applied to the elements listening for a hover event.
    /// During a drag, this is all elements that may shift to adjust for a draggable item to be dropped.
    ListenerElementProperties : IHTMLProp list option

    // todo: remove these if I don't use them.
    Placeholder : Placeholder option

    /// Throttle for elements moving to accomodate a dropped element. 
    /// This won't prevent the first move, only subsequent moves.
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
      Placeholder = None
      MoveThrottleTimeMs = None
    }

  type OnHoverEnter = Browser.Types.MouseEvent -> DraggableId -> Throttle.Id -> unit
  type OnHoverLeave = Browser.Types.MouseEvent -> DraggableId -> Throttle.Id -> unit
  type OnDrop  = Browser.Types.MouseEvent -> DraggableId -> unit

  type MouseEventHandlers = {
    OnHoverEnter : OnHoverEnter option
    OnHoverLeave : OnHoverLeave option
    OnDrop : OnDrop option
  } with
    static member Empty() = {
      OnHoverEnter = None
      OnHoverLeave = None
      OnDrop = None
    }

    member this.GetOnHoverEnter() = this.OnHoverEnter |> Option.defaultValue (fun _ _ _ -> ())
    member this.GetOnHoverLeave() = this.OnHoverLeave |> Option.defaultValue (fun _ _ _ -> ())
    member this.GetOnDrop() = this.OnDrop |> Option.defaultValue (fun _ _ -> ())