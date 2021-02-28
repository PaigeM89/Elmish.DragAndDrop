module App

open Browser.Dom
open Elmish
open Elmish.React
open Fable
open Fable.React
open Fable.React.Props


type Model = {
    Items : string list
    DragonDrop : DragAndDrop.Model
} with
    static member Init() = {
        Items = [ "Item 1"; "Item 2"; "Item 3"; "Item 4"]
        DragonDrop = None
    }

type Msg =
| Initialize
| DragonDrop of DragAndDrop.Msg

let wrapDND dispatch = fun m -> DragonDrop m |> dispatch
let dndDispatch (dispatch: Msg -> unit) = (fun (m : DragAndDrop.Msg) -> DragonDrop m |> dispatch)

let config = {
    DragAndDrop.BeforeUpdate = (fun dragIndex dropIndex li -> li)
    DragAndDrop.Movement = DragAndDrop.Movement.Free
    DragAndDrop.Listen = DragAndDrop.Listen.OnDrag
    DragAndDrop.Operation = DragAndDrop.Operation.Rotate
}

let ghostView (dnd : DragAndDrop.Model) items =
    let mabyeDragItem =
        dnd
        |> Option.map (fun { DragIndex = dragIndex} -> items |> List.skip dragIndex |> List.head )
    match mabyeDragItem with
    | Some item ->
        div (DragAndDrop.ghostStyles config.Movement dnd) [ str item ]
    | None ->
        str ""


let itemView (dnd : DragAndDrop.Model) index item (dispatch : Msg -> unit) =
    let dispatch = dndDispatch dispatch
    let itemId = "id-" + item
    match dnd with
    | Some dragInfo ->
        if dragInfo.DragIndex <> index then
            let dropEvents : IHTMLProp list = (DragAndDrop.dropEvents (fun msg -> msg) (dispatch) index itemId)
            let id : IHTMLProp = HTMLAttr.Id itemId :> IHTMLProp
            p (id :: dropEvents) [str item]
        else
            p [ HTMLAttr.Id itemId ] [ str "[-----------]" ]
    | None ->
        let dragEvents = DragAndDrop.dragEvents id dispatch index itemId
        let id : IHTMLProp = HTMLAttr.Id itemId :> IHTMLProp
        p (id :: dragEvents) [ str item ]

let view (model : Model) (dispatch : Msg -> unit) =
    let dndDispatch = dndDispatch dispatch
    let listeners : IHTMLProp list = DragAndDrop.mouseListener dndDispatch model.DragonDrop
    let items = model.Items |> List.mapi (fun i x -> itemView model.DragonDrop i x dispatch)

    section [
        Style [CSSProp.TextAlign TextAlignOptions.Center]
        yield! listeners
    ] [
        div [] items
        ghostView model.DragonDrop model.Items
    ]

let update (msg : Msg) (model : Model) =
    match msg with
    | Initialize -> model, Cmd.none
    | DragonDrop dragMsg ->
        let dnd, sortedItems = DragAndDrop.update config dragMsg model.DragonDrop model.Items
        let cmds = DragAndDrop.commands DragonDrop dnd
        { model with DragonDrop = dnd; Items = sortedItems }, cmds

let init() =  Model.Init(), Cmd.ofMsg Initialize

Program.mkProgram
    init
    update
    view
|> Program.withReactBatched "app"
|> Program.run
