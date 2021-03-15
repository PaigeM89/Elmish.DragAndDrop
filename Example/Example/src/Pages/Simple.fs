module App.Pages.Simple


open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Elmish.DragAndDrop

// Simple example of sorting items in a list.

type Model = {
    /// Stores Drag state when dragging an item
    DragAndDrop : DragAndDrop.Model
    /// The items to be displayed & sorted
    Items : string list
} with
    static member Init() = {
        // The drag state is "None" when the user is not dragging an item.
        DragAndDrop = None
        Items = [ "Item 1"; "Item 2"; "Item 3"; "Item 4" ]
    }

type Msg =
/// Handle Drag & Drop messages
| DragAndDropMsg of DragAndDrop.Msg

/// Helper function to wrap Drag And Drop messges for dispatching.
let dndDispatch (dispatch: Msg -> unit) = (fun (m : DragAndDrop.Msg) -> DragAndDropMsg m |> dispatch)

/// Configure drag & drop. This could be stored in the model & dynamically changed, but 
/// it wasn't needed for this example.
let dndConfig = {
    DragAndDrop.BeforeUpdate = fun dragIndex dropIndex li -> li
    // Free movement means the item follows the cursor anywhere
    Movement = Movement.Free
    // No buckets means we only care about dragging & dropping this single list.
    Buckets = None
    // On drag means the list changes update as the item is dragged
    //Listen = Listen.OnDrag
    // Rotate is the only operation currently supported. 
    // Rotate is an intuitive item shift to accomodate the dropped item. Shifted items shift to the lower index, 
    // unless they are the last item, in which case they shift upwards.
    //Operation = Operation.Rotate
}

/// Draw the item currently being moved.
let ghostView (dnd : DragAndDrop.Model) items =
    match DragAndDrop.tryGetDraggedItem dnd items with
    | Some item ->
        // The ghost styles will move the item relative to the cursor based on the Movement option you configured.
        div (DragAndDrop.ghostStyles dndConfig.Movement dnd) [ str item ]
    | None ->
        str ""

let itemView (model : Model) index item (dispatch : Msg -> unit) =
    let dispatch = dndDispatch dispatch
    let itemId = "id-" + item |> String.filter(fun x -> x <> ' ')
    let id : IHTMLProp = HTMLAttr.Id itemId :> IHTMLProp
    if isDragActive model.DragAndDrop then
        if DragAndDrop.isDraggedItemByIndex model.DragAndDrop index then
            p [ id ] [ str "[------]" ]
        else
            let dropEvents : IHTMLProp list = DragAndDrop.dropEvents model.DragAndDrop dispatch (ListDrop index) itemId
            p (id :: dropEvents) [ str item ]
    else
        let dragEvents = DragAndDrop.dragEvents dispatch index itemId
        p (id :: dragEvents) [ str item]