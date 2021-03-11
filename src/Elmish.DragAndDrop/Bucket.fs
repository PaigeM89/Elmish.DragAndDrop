module Elmish.DragAndDrop.Bucket

open System
open Browser.Types
open Elmish.DragAndDrop
open Fable.React.Props
open Elmish

type DragIndex = int
type DropIndex =
| ListDrop of int
| BucketDrop

type DragElementId = string
type DropElementId = string

type Position = {
    X : float
    Y : float
}

let private pos x y = {
    X = x
    Y = y
}

/// Internal type to track if the item is being dropped into the list
/// or into a new section
type DropInto =
/// The item is being dropped into the same list it originated in
| SameList
/// The item is being dropped into a different section.
| Bucket

type DragState = {
    DragIndex : DragIndex
    DropIndex : DropIndex
    DragCounter : int
    StartPosition : Position
    CurrentPosition : Position
    DragElementId : DragElementId
    DropElementId : DropElementId
    DragElement: HTMLElement option
    DropElement: HTMLElement option
    /// Define a section to drop items into.
    /// This is useful to move items elsewhere, such as "save for later" for a shopping cart.
    DropInto : DropInto
}

/// Will be 'none' if there is nothing currently being dragged
type Model = DragState option
let isDragActive (m : Model) = Option.isSome m

type Movement =
/// The ghost element follows the cursor
| Free
/// The ghost element follows the cursor but only moves horizontally
| Horizontal
/// The ghost element follows the cursor but only moves vertically.
| Vertical

type Listen = | OnDrag | OnDrop
// Rotate covers the most common use case for drag-and-drop sorting.
type Operation =
/// Shifts elements to accomodate the dropped element. The shifted element will
/// move towards the lower index, unless it is the 0th element, which will shift to the 1st index.
| Rotate
//    | InsertAfter
//    | InsertBefore
//    | Swap

type Msg =
| DragStart of DragIndex * DragElementId * Position
| Drag of Position
| DragOver of DropIndex * DropElementId
/// A hover item being dragged over a section it can be dropped into, instead of the same list it was in.
| DragOverDropSection of DropElementId
| DragEnter of DropIndex
| DragLeave
| DragLeaveDropSection
| DragEnd
//| DragBucketDrop of fn : 'a -> ()
| GotDragElement of Result<HTMLElement, exn>
| GotDropElement of Result<HTMLElement, exn>

let private px x = (string x) + "px"

let private translate x y = "translate3d(" + px x + ", " + px y + ", 0)"

/// Stylize the floating ghost element.
let ghostStyles movement (model : Model) : IHTMLProp list =
    match model with
    | None -> []
    | Some m ->
        match m.DragElement with
        | None -> []
        | Some x ->
            let transform =
                match movement with
                | Horizontal ->
                    let rect  = x.getBoundingClientRect()
                    let y = rect.top - rect.height
                    CSSProp.Transform (translate (m.CurrentPosition.X - m.StartPosition.X + rect.left ) (y))
                | Vertical ->
                    let rect  = x.getBoundingClientRect()
                    let x = rect.left
                    CSSProp.Transform (translate (x) (m.CurrentPosition.Y))
                | Free ->
                    let rect = x.getBoundingClientRect()
                    let x = m.CurrentPosition.X - m.StartPosition.X + rect.left
                    let y = rect.top - rect.height
                    CSSProp.Transform (translate x (m.CurrentPosition.Y) )
            let baseStyles = Style [
                CSSProp.Position PositionOptions.Fixed
                CSSProp.Top "0"
                CSSProp.Left "0"
                CSSProp.Height x.clientHeight
                CSSProp.Width x.clientWidth
                CSSProp.PointerEvents "none"
                CSSProp.Opacity 0.7f
                transform
            ]
            [baseStyles]

type Config<'a> = {
    BeforeUpdate : DragIndex -> DropIndex -> 'a List -> 'a List
    Movement : Movement
    Listen : Listen
    Operation : Operation
}

let private dragElementCommands (dragState : DragState) =
    match dragState.DragElement with
    | None ->
        let doc = Browser.Dom.document
        let ele = doc.getElementById(dragState.DragElementId)
        ele |> Ok |> GotDragElement |> Cmd.ofMsg
    | _ -> Cmd.none

let private dropElementCommands dragState =
    if dragState.DragCounter = 0 && dragState.DragElement.IsSome && dragState.DropElement.IsNone then
        let doc = Browser.Dom.document
        let ele = doc.getElementById(dragState.DropElementId)
        ele |> Ok |> GotDropElement |> Cmd.ofMsg
    else
        Cmd.none

let commands (model : Model) =
    match model with
    | Some m ->
        Cmd.batch[
            dragElementCommands m
            dropElementCommands m
        ]
    | None -> Cmd.none

/// Creates mouse listeners for mouse up (when an item is released)
/// and mouse move (when the ghost item should move with the cursor)
let mouseListener dispatch (model : Model) : IHTMLProp list =
    match model with
    | Some m ->
        [
            OnMouseMove (fun ev -> Drag { X = ev.pageX; Y = ev.pageY } |> dispatch)
            OnMouseUp (fun ev -> printfn "dispatch drag end"; DragEnd |> dispatch)
        ]
    | None -> []


/// Creates a mouse listener event to notify if an item is picked up to be dragged.
let dragEvents dispatch dragIndex dragElementId : IHTMLProp list =
    [
        DOMAttr.OnMouseDown (fun ev ->
            ev.preventDefault()
            (dragIndex, dragElementId, pos (ev.pageX) (ev.pageY))
            |> DragStart
            |> dispatch
        )
    ]

/// Creates events to listen for the mouse entering this index, or for the item being dropped on this index.
let dropEvents model dispatch dropIndex dropElementId : IHTMLProp list =
    if Option.isSome model then
        [
            DOMAttr.OnMouseOver (fun me -> DragOver (dropIndex, dropElementId) |> dispatch)
            DOMAttr.OnMouseEnter (fun me -> (DragEnter dropIndex) |> dispatch)
            DOMAttr.OnMouseLeave (fun me ->  DragLeave |> dispatch)
        ]
    else []

/// Creates mouse listeners to listen for mouse events on a bucket.
/// This contains the regular mouseListener events, but with an added function to handle a dropped item, as well as
/// other specific bucket listeners.
let bucketListeners (model : Model) (dispatch : Msg -> unit) (dropElementId : string) (onDrop) : IHTMLProp list =
    match model with
    | Some m ->
        [
            OnMouseMove (fun ev -> Drag { X = ev.pageX; Y = ev.pageY } |> dispatch)
            OnMouseUp (fun ev -> 
                //DragBucketDrop (onDrop) |> dispatch)
                printfn "dispatch drag end"; onDrop (m.DragIndex); DragEnd |> dispatch
            )
            OnMouseOver (fun me -> DragOverDropSection dropElementId |> dispatch )
            OnMouseLeave (fun me -> DragLeaveDropSection |> dispatch)
        ]
    | None -> []

let private modelUpdate operation dropIndex (model : DragState) =
    match operation with
    | Rotate ->
        // the drag index updates here because entering a new cell makes that cell
        // the "origin" for the item.
        // It's important to remember that once an item is being dragged, it's essentially homeless;
        // the other items shift to accomodate it.
        { model with DragIndex = dropIndex; DragCounter = 0 }

/// Updates the list based on the Operation given.
/// Made public for unit testing. This function is called during the `update`.
let listUpdate op dragIndex dropIndex li =
    match op with
    | Rotate ->
        let split i li =
            let len x = List.length x
            let first =
                if len li > i then List.take i li else li
            let second =
                if len li <= i then [] else  List.skip i li
            first,second

        if dragIndex < dropIndex then
            let beginning, rest = split dragIndex li
            let middle, _end = split (dropIndex - dragIndex + 1) rest
            let head, tail = split 1 middle
            beginning @ tail @ head @ _end
        elif dragIndex > dropIndex then
            let beginning, rest = split dropIndex li
            let middle, _end = split (dragIndex - dropIndex) rest
            let head, tail = split 1 _end
            beginning @ head @ middle @ tail
        else
            li

/// Removes the element with the given DragIndex from the list
/// This is called when updating the list due to an item hovering over or entering
/// the drop bucket and is only visible for unit testing.
let listRemove dragIndex li =
    let len x = List.length x
    if len li >= dragIndex then
        let h = List.take dragIndex li
        let t = List.skip (dragIndex + 1) li
        h @ t
    else
        li

/// Update the Drag And Drop model state, and also return the newly sorted list of items.
let update config msg model (li: 'a list) : (Model * 'a list)=
    match msg with
    | DragStart (dragIndex, dragElementId, { X = x; Y = y }) ->
        {
            DragIndex = dragIndex
            DropIndex = ListDrop dragIndex
            DragCounter = 0
            StartPosition = pos x y
            CurrentPosition = pos x y
            DragElementId = dragElementId
            DropElementId = dragElementId
            DragElement = None
            DropElement = None
            DropInto = SameList
        } |> Some, li
    | Drag { X = x ; Y = y } ->
        model |> Option.map(fun m -> { m with CurrentPosition = pos x y; DragCounter = m.DragCounter + 1 }), li
    | DragOver (dropIndex, dropElementId) ->
        model |> Option.map(fun m -> { m with DropIndex = dropIndex; DropElementId = dropElementId }), li
    | DragOverDropSection dropElementId ->
        match model with
        | Some { DragIndex = di } ->
            model |> Option.map(fun m -> { m with DropElementId = dropElementId; DropInto = Bucket }), li
        | None ->
            model |> Option.map(fun m -> { m with DropElementId = dropElementId; DropInto = Bucket }), li
    | DragEnter (ListDrop dropIndex) ->
        match model,  config.Listen with
        | Some m, OnDrag ->
            if m.DragCounter > 1 && m.DragIndex <> dropIndex then
                let m' = m |> modelUpdate config.Operation dropIndex |> Some
                let newList =
                    li
                    |> config.BeforeUpdate m.DragIndex (ListDrop dropIndex)
                    |> listUpdate config.Operation m.DragIndex dropIndex
                m', newList
            else
                model, li
        | _ -> model |> Option.map (fun x -> { x with DragCounter = 0 }), li
    | DragEnter (BucketDrop) ->
        match model, config.Listen with
        | Some m, OnDrag ->
            if m.DragCounter > 1 then
                let newList =
                    li
                    |> config.BeforeUpdate m.DragIndex (BucketDrop)
                    |> listRemove m.DragIndex
                Some m, newList
            else
                model, li
        | _ -> model |> Option.map (fun x -> { x with DragCounter = 0 }), li
    | DragLeave ->
        model |> Option.map(fun m -> { m with DropIndex = ListDrop m.DragIndex }), li
    | DragLeaveDropSection ->
        model |> Option.map(fun m -> { m with DropIndex = BucketDrop; DropInto = SameList }), li
    | DragEnd ->
        match model, config.Listen with
        | Some m, OnDrop ->
            match m.DropIndex with
            | ListDrop di ->
                if m.DragIndex <> di then
                    let newList =
                        li
                        |> config.BeforeUpdate m.DragIndex m.DropIndex
                        |> listUpdate config.Operation m.DragIndex di
                    None, newList
                else
                    None, li
            | BucketDrop ->
                let newList =
                    li
                    |> config.BeforeUpdate m.DragIndex m.DropIndex
                    |> listRemove m.DragIndex
                None, newList
        | Some m, OnDrag ->
            match m.DropInto with
            | DropInto.Bucket ->
                printfn "drag end with bucket drop"
                let newList =
                    li
                    |> config.BeforeUpdate m.DragIndex m.DropIndex
                    |> listRemove m.DragIndex
                None, newList
            | _ -> None, li
        | _ -> None, li
    // | DragBucketDrop fn ->
    //     match model with
    //     | Some m ->
    //         match m.DropIndex with
    //         | BucketDrop ->
    //             let removedItem = li.[m.DragIndex]
    //             let newList =
    //                 li
    //                 |> config.BeforeUpdate m.DragIndex m.DropIndex
    //                 |> listRemove m.DragIndex
    //             fn removedItem
    //             None, newList
    //         | _ -> None, li
    //     | _ -> None, li
    | GotDragElement (Ok ele) ->
        model |> Option.map(fun m -> { m with DragElement = Some ele; DropElement = Some ele }), li
    | GotDragElement (Error e) ->
        model, li
    | GotDropElement (Ok ele) ->
        model |> Option.map(fun m -> { m with DropElement = Some ele }), li
    | GotDropElement (Error e) ->
        model, li

/// Returns the currently dragged item, or None if no item is being dragged.
let tryGetDraggedItem (model : Model) (items: 'a list) =
    match model with
    | Some { DragIndex = dragIndex} ->
        items |> List.skip dragIndex |> List.tryHead
    | None -> None

/// Returns TRUE if the index is the currently dragged item
let isDraggedItemByIndex model index =
    match model with
    | Some {DragIndex = di} -> di = index
    | None -> false

/// Returns the element ID of the bucket the item is hovered over, or None.
let tryGetDropBucketId model =
    match model with
    | Some { DropInto = Bucket; DropElementId = deId } -> Some deId
    | _ -> None

