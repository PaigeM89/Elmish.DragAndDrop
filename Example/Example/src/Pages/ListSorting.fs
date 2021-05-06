module ListSorting

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Elmish.DragAndDrop

module List =
    let replace item index li = 
        li |> List.mapi (fun i x -> if i = index then item else x)
    
    let remove index li =
        printfn "removing index %i from list %A" index li
        li
        |> List.mapi (fun i x -> if i <> index then Some x else None)
        |> List.choose id

module Map =

    let replace item index m =
        m |> Map.remove index |> Map.add index item

    let choose f m =
        (Map.empty, m)
        ||> Map.fold (fun s k v ->
            let result = f k v
            if Option.isSome result
            then Map.add k result.Value s
            else s
        )

    let chooseValues f m =
        (Map.empty, m)
        ||> Map.fold (fun s k v ->
            let result = f v
            if Option.isSome result
            then Map.add k result.Value s
            else s
        )

type CategoryIndex = int
type Category = {
    Name : string
    Items : string list
    ElementId : string
}

type CategoryType =
| ListSection of Category
| DeleteSection of Category
    with
        member this.IsList() =
            match this with
            | ListSection _ -> true
            | DeleteSection _ -> false

let initCategories() = 
    [
        0, ListSection { Name = "Bucket One"; Items = [ "Item 1"; "Item 2"; "Item 3"; "Item 4"]; ElementId = "bucket-one" }
        1, ListSection { Name = "Bucket Two"; Items = [ "Item 5"; "Item 6"]; ElementId = "bucket-two" }
        2, DeleteSection { Name = "Delete Bucket"; Items = []; ElementId = "delete-bucket" }
    ]
    |> Map.ofList

type Model = {
    /// The items that will be sorted via drag and drop.
    /// If there is an item currently being dragged and/or dropped,
    /// state for that action is stored within this model.
    DragAndDrop : DragAndDrop.Model
    Categories : Map<CategoryIndex, CategoryType>
} with
    static member Init() = {
        Categories = initCategories()
        // Using a single drag & drop model for both sorting lists allows items to possibly transfer between lists.
        // This is because the drag & drop state is shared across the lists.
        DragAndDrop = None
    }

    member this.ListCategories() =
        this.Categories
        |> Map.map (fun k v ->
            match v with
            | ListSection cat -> Some cat
            | DeleteSection _ -> None
        )
        |> Map.chooseValues id

    member this.DeleteCategories() =
        this.Categories
        |> Map.map (fun k v ->
            match v with
            | DeleteSection cat -> Some cat
            | ListSection _ -> None
        )
        |> Map.chooseValues id


type Msg =
| Initialize
/// Handle & dispatch Drag And Drop messages
| DragAndDropMsg of categoryIndex : int * dndMsg : DragAndDrop.Msg
/// Handle bucket changes. As the buckets are defined by the client code (this code), handling how items
/// change between buckets is defined here.
| BucketChange of startBucket : int * startIndex : int * newBucket : int
//| Delete of startBucket : int * startIndex : int

/// Helper function to dispatch Drag And Drop messages to this message handler.
let dndDispatch categoryIndex (dispatch: Msg -> unit) = (fun (m : DragAndDrop.Msg) -> DragAndDropMsg (categoryIndex, m) |> dispatch)

/// Configuration for Drag And Drop functionality. This could be stored in the Model, but
/// that wasn't needed for the example. We use a single config so we can share items between our
/// different lists.
let config = {
    DragAndDrop.BeforeUpdate = (fun dragIndex dropIndex li -> li)
    DragAndDrop.Movement = DragAndDrop.Movement.Free
    /// 2 buckets for the different lists + 1 bucket to delete items
    Buckets = Some 3
}

/// Draw the item currently being moved.
let ghostView (dnd : DragAndDrop.Model) categoryIndex items =
    match tryGetDraggedItem dnd (Some categoryIndex) items with
    | Some item ->
        // The ghost styles will move the item relative to the cursor based on the Movement option you configured.
        div (DragAndDrop.ghostStyles config.Movement dnd) [ str item ]
    | None ->
        str ""

let itemView categoryIndex (model : Model) index item (dispatch : Msg -> unit) =
    let dndModel = model.DragAndDrop
    let onBucketChange (sb : int option) (nb : int option) =
        match sb, nb with
        | Some x, Some y -> BucketChange(x, index, y) |> dispatch
        | _ -> ()
    let dispatch = dndDispatch categoryIndex dispatch
    let itemId = "id-" + item |> String.filter(fun x -> x <> ' ')
    let id : IHTMLProp = HTMLAttr.Id itemId :> IHTMLProp
    if isHoverIndex dndModel index (Some categoryIndex) then
        p [ id ] [ str "[------]" ]
    else
        let dragEvents = DragAndDrop.dragEvents dispatch index itemId (Some categoryIndex)
        let dropEvents : IHTMLProp list = DragAndDrop.dropEvents dndModel dispatch index itemId (Some categoryIndex, onBucketChange)
        p (id :: (dragEvents @ dropEvents)) [ str item ]

let dragAndDropSection categoryIndex model (items : string list) dispatch =
    let dnd = model.DragAndDrop
    // add mouse listeners to track mouse movement & the item drag ending when the item is dropped.
    let listeners : IHTMLProp list = DragAndDrop.mouseListener (dndDispatch categoryIndex dispatch) dnd
    // Note that the map is done with the index.
    let elements = items |> List.mapi (fun i x -> itemView categoryIndex model i x dispatch)

    let isDragBucket() =
        tryGetBucketIndex dnd |> Option.map (fun x -> x = categoryIndex ) |> Option.defaultValue false

    // render the items to be sorted in a section with mouse listeners
    section [
        Style [CSSProp.TextAlign TextAlignOptions.Center]
        yield! listeners
    ] [
        yield div [] elements
    ]

let bucketSection categoryIndex model dispatch _id =
    let drawMovedItem item = p [] [ str item ]
    
    //create some styles when hovering an item over a drop section
    let hoverStyles =
        match tryGetBucketIndex model.DragAndDrop with
        | Some bucketIndex when categoryIndex = bucketIndex ->
            [ CSSProp.Border "1px solid black" ]
        | _ -> [ CSSProp.Border "1px solid white" ]

    section [
        Id _id
        Style [ yield! hoverStyles; yield CSSProp.MarginLeft "25px"; yield CSSProp.MarginRight "25px" ]
    ]

let column model category dispatch categoryIndex =
    let section = bucketSection categoryIndex model dispatch category.ElementId
    let content = dragAndDropSection categoryIndex model category.Items dispatch

    div [
        Style [ CSSProp.ColumnWidth "50%" ]
    ] [
        section [content]
    ]

let deleteBucket model category dispatch ci = 
    let section = bucketSection ci model dispatch category.ElementId
    let content =
        let onBucketChange sb nb = 
            match sb, nb with
            | Some x, Some y -> BucketChange(x, -1, y) |> dispatch
            | _ -> ()
        let dnd = model.DragAndDrop
        let dispatch = (dndDispatch ci dispatch)
        let listeners = DragAndDrop.mouseListener dispatch dnd
        let dropEventListeners = DragAndDrop.dropEvents dnd dispatch 0 "" (Some ci, onBucketChange)
        div [
            Style [ TextAlign TextAlignOptions.Center ]
            yield! (listeners @ dropEventListeners)
        ] [
            h3 [] [ str "Drag here to delete"]
        ]
    
    section [content]

let view (model : Model) (dispatch : Msg -> unit) =
    let listCategories = model.ListCategories()
    let deleteCategoryId, deleteCategory = model.DeleteCategories() |> Map.toList |> List.head
    div [ Style [ CSSProp.TextAlign TextAlignOptions.Center ] ] [
        h2 [] [ str "Drag And Drop to sort or move" ]
        div [ Style [ CSSProp.Display DisplayOptions.InlineFlex ]] [
            yield! listCategories |> Map.toList |> List.map (fun (i, c) -> column model c dispatch i)
            yield! listCategories |> Map.toList |> List.map (fun (i, c) -> ghostView model.DragAndDrop i c.Items)
        ]
        deleteBucket model deleteCategory dispatch deleteCategoryId
    ]


let tryGetCategory model index =
    let x = Map.tryFind index model.Categories
    match x with
    | Some cat -> Some cat
    | None ->
        printfn "Failed to find category with index %i" index
        None

let update (msg : Msg) (model : Model) =
    match msg with
    | Initialize -> model, Cmd.none
    | DragAndDropMsg (categoryIndex, dragMsg) ->
        let maybeCat = Map.tryFind categoryIndex model.Categories
        match maybeCat with
        | Some (ListSection cat) ->
            // The Drag And Drop update will return an updated DND model and a newly sorted list of items.
            let dnd, sortedItems = DragAndDrop.update config dragMsg model.DragAndDrop cat.Items
            let cat = { cat with Items = sortedItems }
            let cats = model.Categories |> Map.replace (ListSection cat) categoryIndex
            // The commands from Drag And Drop need to be fetched separately.
            let cmd = getCommands dnd |> Cmd.map (fun x -> DragAndDropMsg (categoryIndex, x))
            { model with Categories = cats; DragAndDrop = dnd }, cmd
        | _ ->
            printfn "no map found for index %A, handling as delete" categoryIndex
            // no category was found. this must be the delete bucket.
            let dnd, _ = DragAndDrop.update config dragMsg model.DragAndDrop []
            let cmd = getCommands dnd |> Cmd.map (fun x -> DragAndDropMsg (categoryIndex, x))
            { model with DragAndDrop = dnd }, cmd
    | BucketChange (startCatIndex, startIndex, newCatIndex) ->
        printfn "bucket change, sb : %A si %A nb %A" startCatIndex startIndex newCatIndex
        let oldCategoryType = tryGetCategory model startCatIndex
        let newCategoryType = tryGetCategory model newCatIndex
        let originalIndex = tryGetDraggedItemIndex model.DragAndDrop (Some startCatIndex)
        match oldCategoryType, newCategoryType, originalIndex with
        | Some (ListSection cat), Some(ListSection newCat), Some originalIndex ->
            let itemToMove = List.tryItem originalIndex cat.Items
            printfn "original index is %A" originalIndex
            let item = itemToMove |> Option.defaultValue "error: item not found"
            printfn "Item being moved is %A" item
            let oldCat = { cat with Items = List.remove originalIndex cat.Items }
            let newCat = { newCat with Items = item :: newCat.Items }
            let cats =
                model.Categories
                |> Map.replace (ListSection oldCat) startCatIndex
                |> Map.replace (ListSection newCat) newCatIndex
            { model with Categories = cats }, Cmd.none
        | Some (ListSection cat), Some(DeleteSection deleteCat), Some originalIndex ->
            let itemToMove = List.tryItem originalIndex cat.Items
            printfn "original index is %A" originalIndex
            let item = itemToMove |> Option.defaultValue "error: item not found"
            printfn "Item being deleted is %A" item
            let oldCat = { cat with Items = List.remove originalIndex cat.Items }
            let cats =
                model.Categories
                |> Map.replace (ListSection oldCat) startCatIndex
            { model with Categories = cats }, Cmd.none
        | _ ->
            model, Cmd.none



let init() =  Model.Init(), Cmd.ofMsg Initialize
