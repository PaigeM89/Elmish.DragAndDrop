namespace Elmish.DragAndDrop

[<AutoOpen>]
module Model =
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