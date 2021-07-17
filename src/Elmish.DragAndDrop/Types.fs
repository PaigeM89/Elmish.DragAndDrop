namespace Elmish.DragAndDrop2

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
    /// Returns the Ids of each component in the model, in the order they are currently sorted.
    member this.ElementIds() =
      this.Items
      |> List.map (fun itemList ->
        itemList |> List.map (fun (_, _, id) -> string id)
      )

    /// <summary>
    /// Returns the Ids of each component in the model, in the order they are currently sorted.
    /// This appends each category (each root-level list) into a single list, in order of those lists.
    /// If you have a single category, this is entirely harmless and saves you from doing
    /// `ElementIds() |> List.concat` every time you need the element Ids. If you have multiple categories,
    /// then you will usually want `ElementIds()` instead.
    /// </summary>
    member this.ElementIdsSingleList() = this.ElementIds() |> List.concat

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

  let internal getLocationForElement itemId model = 
    model.Items
    |> List.map (fun li -> li |> List.tryFind (fun (_, _, id) -> id = itemId))
    |> List.choose id
    |> List.tryHead

  /// The Id of the throttle
  type MouseEventWithThrottle = Browser.Types.MouseEvent -> Throttle.Id -> unit


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
    BlankPlaceholderStyles : CSSProp list option
    BlankPlaceholderProperties : IHTMLProp list option
    BlankPlaceholderContent : ReactElement option

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
      BlankPlaceholderStyles = None
      BlankPlaceholderProperties = None
      BlankPlaceholderContent = None
      MoveThrottleTimeMs = None
    }