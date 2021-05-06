module Elmish.DragAndDrop

open System
open Browser.Types
open Fable.React.Props
open Elmish

type DragIndex = int
type DropIndex = int

type BucketIndex = int option
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

// todo: when hoving over the other bucket, the ghost changes - lots of things change & break

type DragState = {
    /// The original index of the item being dragged
    StartIndex : DragIndex
    /// The drag index updates as an item is moved, since an item being dragged as no "current" cell, only
    /// a starting cell and the hover cell.
    DragIndex : DragIndex
    DropIndex : DropIndex
    DragCounter : int
    StartPosition : Position
    CurrentPosition : Position
    DragElementId : DragElementId
    DropElementId : DropElementId
    DragElement: HTMLElement option
    DropElement: HTMLElement option

    StartBucket : BucketIndex
    DropBucket : BucketIndex
}

/// Will be 'none' if there is nothing currently being dragged.
type Model = DragState option
let isDragActive (m : Model) = Option.isSome m

type Movement =
/// The ghost element follows the cursor.
| Free
/// The ghost element follows the cursor but only moves horizontally.
| Horizontal
/// The ghost element follows the cursor but only moves vertically.
| Vertical

[<Obsolete>]
type Listen = | OnDrag | OnDrop

// Rotate covers the most common use case for drag-and-drop sorting.
[<Obsolete>]
type Operation =
/// Shifts elements to accomodate the dropped element. The shifted element will
/// move towards the lower index, unless it is the 0th element, which will shift to the 1st index.
| Rotate
//    | InsertAfter
//    | InsertBefore
//    | Swap


type Config<'a> = {
    // todo: revisit this. might not work in here the same way as Elm.
    BeforeUpdate : DragIndex -> DropIndex -> 'a List -> 'a List

    /// The type of Movement to allow for the ghost elements when dragging and dropping.
    Movement : Movement
    /// The number of buckets, or groups, of items. When using a shared drag and drop model, elements can
    /// move between distinct groups. If none or 1, the configured drag and drop model will not share elements
    /// with any other drag & drop containers.
    Buckets : int option
    //Listen : Listen
    //Operation : Operation
    
    /// Run when an item moves to a different bucket than its start bucket.
    /// Arguments are start bucket -> new bucket -> item added to hover bucket.
    /// Note that handling any removal of the item from the start bucket is left to the implementation.
    //OnChangeBucket : (BucketIndex -> BucketIndex -> 'a) option
}

// start bucket -> new bucket -> dispatch
type OnBucketChange = BucketIndex -> BucketIndex -> unit

type Msg =
| DragStart of BucketIndex * DragIndex * DragElementId * Position
| Drag of Position
| DragOver of BucketIndex * DropIndex * DropElementId
/// A hover item being dragged over a section it can be dropped into, instead of the same list it was in.
| DragOverDropSection of DropElementId
| DragEnter of (BucketIndex * OnBucketChange) * DropIndex
| DragLeave
//| DragLeaveDropSection of BucketIndex
| DragEnd
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


/// Creates mouse listeners for mouse up (when an item is released)
/// and mouse move (when the ghost item should move with the cursor)
let mouseListener dispatch (model : Model) : IHTMLProp list =
    match model with
    | Some m ->
        [
            OnMouseMove (fun ev -> Drag { X = ev.pageX; Y = ev.pageY } |> dispatch)
            OnMouseUp (fun ev -> DragEnd |> dispatch)
        ]
    | None -> []


/// Creates a mouse listener event to notify if an item is picked up to be dragged.
let dragEvents dispatch dragIndex dragElementId bucketIndex : IHTMLProp list =
    [
        DOMAttr.OnMouseDown (fun ev ->
            ev.preventDefault()
            (bucketIndex, dragIndex, dragElementId, pos (ev.pageX) (ev.pageY))
            |> DragStart
            |> dispatch
        )
    ]

/// Creates events to listen for the mouse entering this index, or for the item being dropped on this index.
let dropEvents model dispatch dropIndex dropElementId (bucketIndex, onBucketChange) : IHTMLProp list =
    if Option.isSome model then
        [
            DOMAttr.OnMouseOver (fun me -> DragOver (bucketIndex, dropIndex, dropElementId) |> dispatch)
            DOMAttr.OnMouseEnter (fun me -> (DragEnter ((bucketIndex, onBucketChange), dropIndex)) |> dispatch)
            DOMAttr.OnMouseLeave (fun me ->  DragLeave |> dispatch)
        ]
    else []

/// Creates mouse listeners to listen for mouse events on a bucket.
/// This contains the regular mouseListener events, but with an added function to handle a dropped item, as well as
/// other specific bucket listeners.
[<Obsolete>]
let bucketListeners (model : Model) (dispatch : Msg -> unit) (dropElementId : string) (onDrop) : IHTMLProp list =
    match model with
    | Some m ->
        [
            OnMouseMove (fun ev -> Drag { X = ev.pageX; Y = ev.pageY } |> dispatch)
            OnMouseUp (fun ev -> 
                onDrop (m.DragIndex)
                DragEnd |> dispatch
            )
            OnMouseOver (fun me -> DragOverDropSection dropElementId |> dispatch )
            //OnMouseLeave (fun me -> DragLeaveDropSection bucketIndex |> dispatch)
        ]
    | None -> []

let private modelUpdate dropIndex (model : DragState) =
    // the drag index updates here because entering a new cell makes that cell the "origin" for the item.
    // It's important to remember that once an item is being dragged, it's essentially homeless;
    // the other items shift to accomodate it.
    { model with DragIndex = dropIndex; DragCounter = 0 }

let private moveBuckets dropIndex bucketIndex (model : DragState) =
    printfn "moving item to other bucket %A with DI %i"  bucketIndex dropIndex
    { model with DropIndex = dropIndex; DragCounter = 0; DropBucket = bucketIndex }

/// Updates the list.
/// Made public for unit testing. This function is called during the `update`.
let listUpdate dragIndex dropIndex li =
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
let update config msg model (li: 'a list) : (Model * 'a list) =
    match msg with
    | DragStart (bucketIndex, dragIndex, dragElementId, { X = x; Y = y }) ->
        {
            StartIndex = dragIndex
            DragIndex = dragIndex
            DropIndex = dragIndex
            DragCounter = 0
            StartPosition = pos x y
            CurrentPosition = pos x y
            DragElementId = dragElementId
            DropElementId = dragElementId
            DragElement = None
            DropElement = None

            StartBucket = bucketIndex
            DropBucket = bucketIndex
        } |> Some, li
    | Drag { X = x ; Y = y } ->
        model |> Option.map(fun m -> { m with CurrentPosition = pos x y; DragCounter = m.DragCounter + 1 }), li
    | DragOver (bucketIndex, dropIndex, dropElementId) ->
        model |> Option.map(fun m -> { m with DropIndex = dropIndex; DropElementId = dropElementId; DropBucket = bucketIndex }), li
    | DragOverDropSection dropElementId ->
        match model with
        | Some { DragIndex = di } ->
            model |> Option.map(fun m -> { m with DropElementId = dropElementId }), li
        | None ->
            model |> Option.map(fun m -> { m with DropElementId = dropElementId }), li
    | DragEnter ((bucketIndex, onBucketChange), dropIndex) ->
        printfn "In drag enter, bucket %A, drop index %A" bucketIndex dropIndex
        match model with
        | Some m ->
            printfn "in drag enter, model is %A" model
            // only change list order if we're in the same bucket ( verify later ? doesn't allow insertion at an index)
            if m.DropBucket = bucketIndex && m.DragCounter > 1 && m.DragIndex <> dropIndex then
                let m' = m |> modelUpdate dropIndex |> Some
                let newList =
                    li
                    |> config.BeforeUpdate m.DragIndex dropIndex
                    |> listUpdate m.DragIndex dropIndex
                m', newList
            elif m.DragCounter > 1 && m.DropBucket <> bucketIndex then
                onBucketChange m.DropBucket bucketIndex
                let newList =
                    if dropIndex > 0 then
                        li
                        |> config.BeforeUpdate m.DragIndex dropIndex
                        |> listUpdate m.DragIndex dropIndex
                    else
                        li
                
                moveBuckets dropIndex bucketIndex m |> Some, newList
            else
                model, li
        | _ ->
            printfn "in drag enter, no model"
            model |> Option.map (fun x -> { x with DragCounter = 0 }), li
    | DragLeave ->
        model |> Option.map(fun m -> { m with DropIndex = m.DragIndex }), li
    | DragEnd ->
        None, li
    | GotDragElement (Ok ele) ->
        model |> Option.map(fun m -> { m with DragElement = Some ele; DropElement = Some ele }), li
    | GotDragElement (Error e) ->
        model, li
    | GotDropElement (Ok ele) ->
        model |> Option.map(fun m -> { m with DropElement = Some ele }), li
    | GotDropElement (Error e) ->
        model, li

/// Helper functions to interact with the Drag And Drop Model in a more abstract way.
[<AutoOpen>]
module ExternalHelpers =

    /// Returns the Id of the currently dragged item, or None if no item is being dragged.
    let tryGetDraggedItemId (model : Model) = model |> Option.map (fun m -> m.DragElementId)

    /// Returns the currently dragged item from the list by index.
    /// Returns None if no item is being dragged or the drag index exceeds the item list length.
    let tryGetDraggedItem (model : Model) bucketIndex (items: 'a list) =
        let len = items |> List.length
        match model, bucketIndex with
        | Some { DragIndex = dragIndex; DropBucket = (Some sb) }, Some bucketId when len > dragIndex && sb = bucketId ->
            items |> List.skip dragIndex |> List.tryHead
        | Some { DragIndex = dragIndex; StartBucket = sb }, None when len > dragIndex && Option.isNone sb ->
            items |> List.skip dragIndex |> List.tryHead
        | _ -> None

    /// Returns TRUE if the index & bucket is the currently dragged item.
    let isDraggedItemByIndex model index bucketIndex =
        match model with
        | Some {DragIndex = di; StartBucket = sb} -> 
            (di = index) && (sb = bucketIndex)
        | None -> false

    let tryGetDraggedItemIndex model bucketIndex =
        match model with
        | Some { DragIndex = di; StartBucket = sb} when sb = bucketIndex->
            Some di
        | _ -> None

    /// Returns TRUE if the index & bucket is the item currently being hovered over.
    let isHoverIndex model index bucketIndex =
        match model with
        | Some { DragIndex = di; DropBucket = db } ->
            (di = index) && (bucketIndex = db)
        | None -> false

    /// Returns the index of the bucket being hovered over.
    /// Returns None if either the buckets are not defined (which happens with a single bucket),
    /// or if there is no drag occurring.
    let tryGetBucketIndex model = 
        match model with
        | Some { DropBucket = db } -> db
        | None -> None

    let tryGetOriginalDragIndex model =
        match model with
        | Some { StartIndex = si } -> Some si
        | None -> None

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

    let getCommands (model : Model) =
        match model with
        | Some m ->
            Cmd.batch[
                dragElementCommands m
                dropElementCommands m
            ]
        | None -> Cmd.none