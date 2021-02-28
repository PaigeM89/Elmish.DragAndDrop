module Elmish.DragAndDrop

    open Browser.Types
    open Fable.React.Props

    type DragIndex = int
    type DropIndex = int
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
    }
    /// Will be 'none' if there is nothing currently being dragged
    type Model = DragState option

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
//    | InsertAfter
//    | InsertBefore
    /// Shifts elements to accomodate the dropped element. The shifted element will
    /// move towards the lower index, unless it is the 0th element, which will shift to the 1st index.
    | Rotate
//    | Swap
//    | Unaltered

    type Msg =
    | DragStart of DragIndex * DragElementId * Position
    | Drag of Position
    | DragOver of DropIndex * DropElementId
    | DragEnter of DropIndex
    | DragLeave
    | DragEnd
    | GotDragElement of Result<HTMLElement, exn >
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

    let private dragElementCommands stepMsg (dragState : DragState) =
        match dragState.DragElement with
        | None ->
            let doc = Browser.Dom.document
            let ele = doc.getElementById(dragState.DragElementId)
            ele |> Ok |> GotDragElement |> stepMsg |> Cmd.ofMsg
        | _ -> Cmd.none

    let private dropElementCommands stepMsg dragState =
        if dragState.DragCounter = 0 && dragState.DragElement.IsSome && dragState.DropElement.IsNone then
            let doc = Browser.Dom.document
            let ele = doc.getElementById(dragState.DropElementId)
            ele |> Ok |> GotDropElement |> stepMsg |> Cmd.ofMsg
        else
            Cmd.none

    let commands stepMsg (model : Model) =
        match model with
        | Some m ->
            Cmd.batch[
                dragElementCommands stepMsg m
                dropElementCommands stepMsg m
            ]
        | None -> Cmd.none

    /// Creates mouse listeners for mouse up (when an item is released)
    /// and mouse move (when the ghost item should move with the cursor)
    let mouseListener stepMsg (model : Model) : IHTMLProp list =
        match model with
        | Some m ->
            [
                OnMouseMove (fun ev -> Drag { X = ev.pageX; Y = ev.pageY } |> stepMsg)
                OnMouseUp (fun ev -> DragEnd |> stepMsg)
            ]
        | None -> []


    /// Creates a mouse listener event to notify if an item is picked up to be dragged.
    let dragEvents stepMsg dispatch dragIndex dragElementId : IHTMLProp list =
        [
            DOMAttr.OnMouseDown (fun ev ->
                ev.preventDefault()
                (dragIndex, dragElementId, pos (ev.pageX) (ev.pageY))
                |> DragStart
                |> stepMsg
                |> dispatch
            )
        ]

    /// Creates events to listen for the mouse entering this index, or for the item being dropped on this index.
    let dropEvents (stepMsg: Msg -> Msg) dispatch dropIndex dropElementId : IHTMLProp list =
        [
            DOMAttr.OnMouseOver (fun me -> (DragOver (dropIndex, dropElementId)) |> dispatch)
            DOMAttr.OnMouseEnter (fun me -> stepMsg (DragEnter dropIndex) |> dispatch)
            DOMAttr.OnMouseLeave (fun me -> stepMsg DragLeave |> dispatch)
        ]

    let private modelUpdate operation dropIndex (model : DragState) =
        match operation with
//        | InsertAfter ->
//            { model with DragIndex = (if dropIndex < model.DragIndex then dropIndex + 1 else dropIndex); DragCounter = 0 }
//        | InsertBefore ->
//            { model with DragIndex = (if dropIndex < model.DragIndex then dropIndex - 1 else dropIndex); DragCounter = 0 }
        | Rotate ->
            // the drag index updates here because entering a new cell makes that cell
            // the "origin" for the item.
            // It's important to remember that once an item is being dragged, it's essentially homeless;
            // the other items shift to accomodate it.
            { model with DragIndex = dropIndex; DragCounter = 0 }

    let private listUpdate op dragIndex dropIndex li =
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

    /// Update the Drag And Drop model state, and also return the newly sorted list of items.
    let update config msg model (li: 'a list) : (Model * 'a list)=
        match msg with
        | DragStart (dragIndex, dragElementId, { X = x; Y = y }) ->
            {
                DragIndex = dragIndex
                DropIndex = dragIndex
                DragCounter = 0
                StartPosition = pos x y
                CurrentPosition = pos x y
                DragElementId = dragElementId
                DropElementId = dragElementId
                DragElement = None
                DropElement = None
            } |> Some, li
        | Drag { X = x ; Y = y } ->
            model |> Option.map(fun m -> { m with CurrentPosition = pos x y; DragCounter = m.DragCounter + 1 }), li
        | DragOver (dropIndex, dropElementId) ->
            model |> Option.map(fun m -> { m with DropIndex = dropIndex; DropElementId = dropElementId }), li
        | DragEnter dropIndex ->
            match model, config.Listen with
            | Some m, OnDrag ->
                if m.DragCounter > 1 && m.DragIndex <> dropIndex then
                    let m' = m |> modelUpdate config.Operation dropIndex |> Some
                    let newList =
                        li
                        |> config.BeforeUpdate m.DragIndex dropIndex
                        |> listUpdate config.Operation m.DragIndex dropIndex
                    m', newList
                else
                    model, li
            | _ -> model |> Option.map (fun x -> { x with DragCounter = 0 }), li
        | DragLeave ->
            model |> Option.map(fun m -> { m with DropIndex = m.DragIndex }), li
        | DragEnd ->
            match model, config.Listen with
            | Some m, OnDrop ->
                if m.DragIndex <> m.DropIndex then
                    let newList =
                        li
                        |> config.BeforeUpdate m.DragIndex m.DropIndex
                        |> listUpdate config.Operation m.DragIndex m.DropIndex
                    None, newList
                else
                    None, li
            | _ -> None, li
        | GotDragElement (Ok ele) ->
            model |> Option.map(fun m -> { m with DragElement = Some ele; DropElement = Some ele }), li
        | GotDragElement (Error e) ->
            model, li
        | GotDropElement (Ok ele) ->
            model |> Option.map(fun m -> { m with DropElement = Some ele }), li
        | GotDropElement (Error e) ->
            model, li
