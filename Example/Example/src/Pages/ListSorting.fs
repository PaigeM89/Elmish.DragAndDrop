module App.Pages.ListSorting

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Elmish.DragAndDrop
open Elmish.DragAndDrop.ListSorting

type Model = {
    /// The items that will be sorted via drag and drop.
    Items : string list
    /// If there is an item currently being dragged and/or dropped,
    /// state for that action is stored within this model.
    ListSorting : ListSorting.Model
    MovedItems : string list
} with
    static member Init() = {
        Items = [ "Item 1"; "Item 2"; "Item 3"; "Item 4"]
        MovedItems = []
        ListSorting = None
    }

type Msg =
| Initialize
/// Handle & dispatch Drag And Drop messages
| ListSorting of ListSorting.Msg

/// Helper function to dispatch Drag And Drop messages to this message handler.
let dndDispatch (dispatch: Msg -> unit) = (fun (m : ListSorting.Msg) -> ListSorting m |> dispatch)

/// Configuration for Drag And Drop functionality. This could be stored in the Model, but
/// that wasn't needed for the example.
let config = {
    ListSorting.BeforeUpdate = (fun dragIndex dropIndex li -> li)
    ListSorting.Movement = Movement.Free
    ListSorting.Listen = Listen.OnDrag
    ListSorting.Operation = Operation.Rotate
}

/// Draw the item currently being moved.
let ghostView (dnd : ListSorting.Model) items =
    let mabyeDragItem =
        ListSorting.tryGetDraggedItem dnd items
    match mabyeDragItem with
    | Some item ->
        // The ghost styles will move the item relative to the cursor based on the Movement option you configured.
        div (ListSorting.ghostStyles config.Movement dnd) [ str item ]
    | None ->
        str ""


let itemView (dnd : ListSorting.Model) index item (dispatch : Msg -> unit) =
    let dispatch = dndDispatch dispatch
    let itemId = "id-" + item |> String.filter(fun x -> x <> ' ')
    match dnd with
    | Some dragState ->
        if dragState.DragIndex <> index then
            // if the current item is not the item being dragged, add events to listen for the item being dropped.
            let dropEvents : IHTMLProp list = (ListSorting.dropEvents (dispatch) index itemId)
            let id : IHTMLProp = HTMLAttr.Id itemId :> IHTMLProp
            p (id :: dropEvents) [str item]
        else
            // Render the placeholder if the item is being hovered over this index.
            p [ HTMLAttr.Id itemId ] [ str "[-----------]" ]
    | None ->
        // Get the events for drag & drop to add them to this item.
        let dragEvents = ListSorting.dragEvents dispatch index itemId
        let id : IHTMLProp = HTMLAttr.Id itemId :> IHTMLProp
        p (id :: dragEvents) [ str item ]

let dragAndDropSection model dispatch =
    // add mouse listeners to track mouse movement & the item drag ending when the item is dropped.
    let listeners : IHTMLProp list = ListSorting.mouseListener (dndDispatch dispatch) model.ListSorting
    // Note that the map is done with the index.
    let items = model.Items |> List.mapi (fun i x -> itemView model.ListSorting i x dispatch)

    // render the items to be sorted in a section with mouse listeners
    section [
        Style [CSSProp.TextAlign TextAlignOptions.Center]
        yield! listeners
    ] [
        div [] items
        ghostView model.ListSorting model.Items
    ]

let dropSection model dispatch =
    let listeners : IHTMLProp list = ListSorting.mouseListener (dndDispatch dispatch) model.ListSorting
    let drawMovedItem item =
        p [] [ str item ]
    let _id = "drop-section"
    section [
        Id _id
        // listeners are required to move the hover item to this section.
        yield! listeners
        //yield! DragAndDrop.dropIntoEvents (dndDispatch dispatch) _id
    ] [
        yield h3 [ Style [CSSProp.TextAlign TextAlignOptions.Center] ] [ str "Drop Items Here" ]
        for item in model.MovedItems do yield drawMovedItem item
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    div [] [
        h2 [ Style [ CSSProp.TextAlign TextAlignOptions.Center ]  ] [ str "Drag And Drop to sort or move" ]
        dragAndDropSection model dispatch
        dropSection model dispatch
    ]

let update (msg : Msg) (model : Model) =
    match msg with
    | Initialize -> model, Cmd.none
    | ListSorting dragMsg ->
        // The Drag And Drop update will return an updated DND model and a newly sorted list of items.
        let dnd, sortedItems = ListSorting.update config dragMsg model.ListSorting model.Items
        // The commands from Drag And Drop need to be fetched separately.
        let cmd = ListSorting.commands dnd |> Cmd.map ListSorting
        { model with ListSorting = dnd; Items = sortedItems }, cmd

let init() =  Model.Init(), Cmd.ofMsg Initialize
