module Elmish.DragAndDrop

    open System.Net
    open Fable.Core
    open Browser.Dom
    open Browser.DomExtensions
    open Browser.Types
    open Fable.React.Props
    open Fable.React.ReactiveComponents

    type DragIndex = int
    type DropIndex = int
    type DragElementId = string
    type DropElementId = string

    type Position = {
        X : float
        Y : float
    }

    let pos x y = {
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
    // Will be 'none' if there is nothing currently being dragged
    type Model = DragState option

    type Movement = | Free | Horizontal | Vertical
    type Listen = | OnDrag | OnDrop
    type Operation =
    | InsertAfter
    | InsertBefore
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

    let px x = (string x) + "px"

    let translate x y =
        "translate3d(" + px x + ", " + px y + ", 0)"

    let ghostStyles movement (model : Model) : IHTMLProp list =
        match model with
        | None -> []
        | Some m ->
            match m.DragElement with
            | None ->
                printfn "dragging with no drag element"
                []
            | Some x ->
                let transform =
                    match movement with
                    | Horizontal ->
                        let z = x.clientTop - x.offsetTop
                        CSSProp.Transform (translate (m.CurrentPosition.X - m.StartPosition.X) (z))
                    | Vertical ->
                        let rect  = x.getBoundingClientRect()
                        let z = x.clientTop - x.offsetTop
                        let diff = rect.left
                        CSSProp.Transform (translate (diff) (m.CurrentPosition.Y - m.StartPosition.Y - rect.top ))
                    | Free ->
                        let trans = CSSProp.Transform (translate (m.CurrentPosition.X - m.StartPosition.X) (m.CurrentPosition.Y - m.StartPosition.Y))
                        printfn "transform is %A" trans
                        trans
                printfn "transform is %A" transform
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

//    type Info = {
//        DragIndex : DragIndex
//        DropIndex : DropIndex
//        StartPosition : Position
//        CurrentPosition : Position
//        DragElementId : DragElementId
//        DropElementId : DropElementId
//        DragElement : HTMLElement
//        DropElement : HTMLElement
//    }
//
//    let info (model : Model) =
//        model |> Option.map(fun m ->
//            Option.map2(fun dragElement dropElement ->
//                {
//                    DragIndex = m.DragIndex
//                    DropIndex = m.DropIndex
//                    DragElementId = m.DragElementId
//                    DropElementId = m.DropElementId
//                    DragElement = dragElement
//                    DropElement = dropElement
//                    StartPosition = m.StartPosition
//                    CurrentPosition = m.CurrentPosition
//                }
//            ) m.DragElement m.DropElement
//        ) |> Option.flatten


    let dragElementCommands stepMsg (dragState : DragState) =
        match dragState.DragElement with
        | None ->
            let doc = Browser.Dom.document
            let ele = doc.getElementById(dragState.DragElementId)
            printfn "setting drag element to %A" ele.id
            ele |> Ok |> GotDragElement |> stepMsg |> Cmd.ofMsg
            //Cmd.none
        | _ -> Cmd.none

    let dropElementCommands stepMsg dragState =
        if dragState.DragCounter = 0 && dragState.DragElement.IsSome && dragState.DropElement.IsNone then
            let doc = Browser.Dom.document
            let ele = doc.getElementById(dragState.DropElementId)
            printfn "setting drop element to %A" ele.id
            ele |> Ok |> GotDropElement |> stepMsg |> Cmd.ofMsg
            //Cmd.none
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

    let mouseListener stepMsg (model : Model) : IHTMLProp list =
        match model with
        | Some m ->
            [
                OnMouseMove (fun ev ->
                    Drag { X = ev.pageX; Y = ev.pageY } |> stepMsg)
                OnMouseUp (fun ev -> DragEnd |> stepMsg)
            ]
        | None -> []


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

    let dropEvents (stepMsg: Msg -> Msg) dispatch dropIndex dropElementId : IHTMLProp list =
        [
            DOMAttr.OnMouseOver (fun me ->
                printfn "in mouse over"
                (DragOver (dropIndex, dropElementId)) |> dispatch)
            DOMAttr.OnMouseEnter (fun me -> stepMsg (DragEnter dropIndex) |> dispatch)
            DOMAttr.OnMouseLeave (fun me -> stepMsg DragLeave |> dispatch)
        ]

    let modelUpdate operation dropIndex (model : DragState) =
        match operation with
        | InsertAfter ->
            { model with DragIndex = (if dropIndex < model.DragIndex then dropIndex + 1 else dropIndex); DragCounter = 0 }
        | InsertBefore ->
            { model with DragIndex = (if dropIndex < model.DragIndex then dropIndex - 1 else dropIndex); DragCounter = 0 }
        | Rotate ->
            // the drag index updates here because entering a new cell makes that cell
            // the "origin" for the item.
            // It's important to remember that once an item is being dragged, it's essentially homeless;
            // the other items shift to accomodate it.
            { model with DragIndex = dropIndex; DragCounter = 0 }

    let listUpdate op dragIndex dropIndex li =
        match op with
        | InsertAfter ->
            if dragIndex < dropIndex then
                let beginning, rest = List.splitAt (dropIndex + 1) li
                let middle, _end = List.splitAt (dragIndex - dropIndex - 1) rest
                let head, tail = List.splitAt 1 _end
                beginning @ head @ middle @ tail
            else if dropIndex < dragIndex then
                let beginning, rest = List.splitAt (dropIndex + 1) li
                let middle, _end = List.splitAt (dragIndex - dropIndex - 1) rest
                let head, tail = List.splitAt 1 _end
                beginning @ head @ middle @ tail
            else li
        | InsertBefore ->
            printfn "inserting before"
            if dragIndex < dropIndex then
                let beginning, rest = List.splitAt dragIndex li
                let middle, _end = List.splitAt (dropIndex - dragIndex) rest
                let head, tail = List.splitAt 1 middle
                let li' = beginning @ tail @ head @ _end
                printfn "List before: %A.\nList After: %A" li li'
                li'
            else if dropIndex < dragIndex then
                let beginning, rest = List.splitAt dragIndex li
                let middle, _end = List.splitAt (dragIndex - dropIndex - 1) rest
                let head, tail = List.splitAt 1 _end
                let li' = beginning @ head @ middle @ tail
                printfn "List before: %A.\nList After: %A" li li'
                li'
            else li
        | Rotate ->
            let split i li =
                let len x = List.length x
                let first =
                    if len li > i then List.take i li else li
                let second =
                    if len li <= i then [] else  List.skip i li

                first,second

            printfn "in rotate; drag index is %A and drop index is %A\n" dragIndex dropIndex
            if dragIndex < dropIndex then
                let beginning, rest = split dragIndex li
                let middle, _end = split (dropIndex - dragIndex + 1) rest
                let head, tail = split 1 middle
                beginning @ tail @ head @ _end
            elif dragIndex > dropIndex then
                let beginning, rest = split dropIndex li
                let middle, _end = split (dragIndex - dropIndex) rest
                let head, tail = split 1 _end
                let li' = beginning @ head @ middle @ tail
                printfn "List before: %A.\nList After: %A\n" li li'
                li'
            else
                li

    let update config msg model (li: 'a list) : (Model * 'a list)=
        //printfn "update in library. msg is %A model is %A config is %A list is %A" msg model config li
        match msg with
        | DragStart (dragIndex, dragElementId, { X = x; Y = y }) ->
            printfn "drag start, drag index is %A, drag element ID is %A" dragIndex dragElementId
            {
                DragIndex = dragIndex
                //HoverIndex = dragIndex
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
            printfn "drag over, drop index is %A" dropIndex
            model |> Option.map(fun m -> { m with DropIndex = dropIndex; DropElementId = dropElementId }), li
        | DragEnter dropIndex ->
            printfn "drag enter"
            match model, config.Listen with
            | Some m, OnDrag ->
                printfn "listening on drag, dragindex is %A, dropindex is %A" m.DragIndex dropIndex //m.HoverIndex
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
            printfn "drag leave"
            // was drag index
            model |> Option.map(fun m -> { m with DropIndex = m.DragIndex }), li
        | DragEnd ->
            printfn "drag end. model is %A" model
            match model, config.Listen with
            | Some m, OnDrop ->
                printfn "dragindex is %A, dropindex is %A" m.DragIndex m.DropIndex
                if m.DragIndex <> m.DropIndex then
                    let newList =
                        li
                        |> config.BeforeUpdate m.DragIndex m.DropIndex
                        |> listUpdate config.Operation m.DragIndex m.DropIndex
                    printfn "list is %A, newList is %A" li newList
                    None, newList
                else
                    None, li
            | _ -> None, li
        | GotDragElement (Ok ele) ->
            printfn "got drag element"
            model |> Option.map(fun m -> { m with DragElement = Some ele; DropElement = Some ele }), li
        | GotDragElement (Error e) ->
            printfn "got drag element error: %A" e
            model, li
        | GotDropElement (Ok ele) ->
            printfn "got drop element"
            model |> Option.map(fun m -> { m with DropElement = Some ele }), li
        | GotDropElement (Error e) ->
            printfn "got drop element error: %A" e
            model, li



//    type System<'a, 'm> =
//        {
//            Model : Model
////            //Subscriptions : Model -> Sub 'msg
//            Commands : Model -> Cmd<'m>
//            Update : 'm -> Model -> 'a list -> (Model * 'a list)
//            DragEvents : DragIndex -> DragElementId -> (DOMAttr) list
//            DropEvents : DropIndex -> DropElementId -> (DOMAttr) list
////            GhostStyles : Model -> (Html.Attribute msg) list
//            Info : Model -> Info option
//        } with
//            static member Create<'a> config dispatch (stepMsg : Msg -> 'a) = {
//                Model = None
//                Commands = commands stepMsg
//                Update = update config
//                DragEvents = dragEvents stepMsg dispatch
//                DropEvents = dropEvents stepMsg dispatch
//                Info = info
//            }
