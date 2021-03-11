module App.Pages.Bucket

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Elmish.DragAndDrop
open Elmish.DragAndDrop.Bucket

type Model = {
    /// The items that will be sorted via drag and drop.
    Items : string list
    /// If there is an item currently being dragged and/or dropped,
    /// state for that action is stored within this model.
    BucketDrop : Bucket.Model
    MovedItems : string list
} with
    static member Init() = {
        Items = [ "Item 1"; "Item 2"; "Item 3"; "Item 4"]
        MovedItems = []
        BucketDrop = None
    }

type Msg =
| Initialize
/// Handle & dispatch Drag And Drop messages
| DragonDrop of Bucket.Msg
| AddToBucketByIndex of index : int
| AddToBucket of value : string

/// Helper function to dispatch Drag And Drop messages to this message handler.
let dndDispatch (dispatch: Msg -> unit) = (fun (m : Bucket.Msg) -> DragonDrop m |> dispatch)

/// Configuration for Drag And Drop functionality. This could be stored in the Model, but
/// that wasn't needed for the example.
let config = {
    Bucket.BeforeUpdate = (fun dragIndex dropIndex li -> li)
    Bucket.Movement = Movement.Free
    Bucket.Listen = Listen.OnDrag
    Bucket.Operation = Operation.Rotate
}

/// Draw the item currently being moved.
let ghostView (dnd : Bucket.Model) items =
    let mabyeDragItem =
        Bucket.tryGetDraggedItem dnd items
    match mabyeDragItem with
    | Some item ->
        // The ghost styles will move the item relative to the cursor based on the Movement option you configured.
        div (Bucket.ghostStyles config.Movement dnd) [ str item ]
    | None ->
        str ""

let itemView (model : Model) index item (dispatch : Msg -> unit) =
    let dispatch = dndDispatch dispatch
    let itemId = "id-" + item |> String.filter(fun x -> x <> ' ')
    let id : IHTMLProp = HTMLAttr.Id itemId :> IHTMLProp
    if isDragActive model.BucketDrop then
        if Bucket.isDraggedItemByIndex model.BucketDrop index then
            p [ id ] [ str "[------]" ]
        else
            let dropEvents : IHTMLProp list = Bucket.dropEvents model.BucketDrop dispatch (ListDrop index) itemId
            p (id :: dropEvents) [ str item ]
    else
        let dragEvents = Bucket.dragEvents dispatch index itemId
        p (id :: dragEvents) [ str item]

let dragAndDropSection model dispatch =
    // add mouse listeners to track mouse movement & the item drag ending when the item is dropped.
    let listeners : IHTMLProp list = Bucket.mouseListener (dndDispatch dispatch) model.BucketDrop
    // Note that the map is done with the index.
    let items = model.Items |> List.mapi (fun i x -> itemView model i x dispatch)

    // render the items to be sorted in a section with mouse listeners
    section [
        Style [CSSProp.TextAlign TextAlignOptions.Center]
        yield! listeners
    ] [
        div [] items
        ghostView model.BucketDrop model.Items
    ]

let dropSection model dispatch _id =
    let drawMovedItem item = p [] [ str item ]
    let baseStyles = [ (CSSProp.TextAlign TextAlignOptions.Center) ]
    
    //create some styles when hovering an item over a drop section
    let hoverStyles =
        match Bucket.tryGetDropBucketId model.BucketDrop with
        | Some _hoverId when _id = _hoverId ->
            [ CSSProp.Border "1px solid black" ]
        | _ -> [ CSSProp.Border "1px solid white" ]

    let dispatchDrop e =
        AddToBucketByIndex e |> dispatch

    section [
        Id _id

        // Add listeners for the drop bucket.
        yield! Bucket.bucketListeners (model.BucketDrop) (dndDispatch dispatch) _id (fun x -> dispatchDrop x)
        Style [ yield! hoverStyles ]
    ] [
        yield h3 [ Style baseStyles ] [ str "Drop Items Here" ]
        for item in model.MovedItems do yield drawMovedItem item
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    div [] [
        h2 [ Style [ CSSProp.TextAlign TextAlignOptions.Center ]  ] [ str "Drag And Drop to sort or move" ]
        dropSection model dispatch "drop-section-1"
        dragAndDropSection model dispatch
        dropSection model dispatch "drop-section-2"
    ]

let update (msg : Msg) (model : Model) =
    match msg with
    | Initialize -> model, Cmd.none
    | DragonDrop (DragOverDropSection elementId) ->
        // The Drag And Drop update will return an updated DND model and a newly sorted list of items.
        let dnd, sortedItems = Bucket.update config (DragOverDropSection elementId) model.BucketDrop model.Items
        // The commands from Drag And Drop need to be fetched separately.
        let cmd = Bucket.commands dnd |> Cmd.map DragonDrop
        { model with BucketDrop = dnd; Items = sortedItems }, cmd
    | DragonDrop dragMsg ->
        // The Drag And Drop update will return an updated DND model and a newly sorted list of items.
        let dnd, sortedItems = Bucket.update config dragMsg model.BucketDrop model.Items
        // The commands from Drag And Drop need to be fetched separately.
        let cmd = Bucket.commands dnd |> Cmd.map DragonDrop
        { model with BucketDrop = dnd; Items = sortedItems }, cmd
    | AddToBucketByIndex index ->
        let item = List.tryItem index model.Items
        printfn "Item added to bucket is %A" item
        match item with
        | Some i -> 
            let li = i :: model.MovedItems
            { model with MovedItems = li}, Cmd.none
        | None ->
            printfn "Unable to find moved item"
            model, Cmd.none

let init() =  Model.Init(), Cmd.ofMsg Initialize
