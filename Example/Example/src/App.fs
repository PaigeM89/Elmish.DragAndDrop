module App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props

type Page =
| RateLimiting of model :  RateLimiting.Model
| DragAndDrop2 of model : Pages.CollectionDragAndDrop2.Model
| SingleListDemo of model : Pages.SingleListDemo.Model
| MultiListDemo of model : Pages.MultiListDemo.Model
| HandlesDemo of model : Pages.HandlesDemo.Model
| HorizontalDemo of model : Pages.HorizontalDemo.Model
| Sliding of model : Pages.Sliding.React.Model

type Model = {
    Page : Page
}

let init() =
    let m = Pages.CollectionDragAndDrop2.init()
    { Page = DragAndDrop2 m}, Cmd.none

type Msg =
| RateLimitingMsg of RateLimiting.Msg
| ToDragAndDrop
| ToDragAndDrop3
| DragAndDrop2Msg of Pages.CollectionDragAndDrop2.Msg
| SingleListDemoMsg of Pages.SingleListDemo.Msg
| MultiListDemoMsg of Pages.MultiListDemo.Msg
| HorizontalDemoMsg of Pages.HorizontalDemo.Msg
| HandlesDemoMsg of Pages.HandlesDemo.Msg
| ToSingleListDemo
| ToMultiListDemo
| ToHandlesDemo
| ToHorizontalDemo
| ToSliding
| SlidingMsg of Pages.Sliding.React.Msg

[<Feliz.ReactComponent>]
let Root(m : Model) =
    Pages.Sliding.React.view m (fun _ ->())

let view model (dispatch : Msg -> unit) =
    div [] [
        yield h2 [Style [ CSSProp.TextAlign TextAlignOptions.Center ] ] [ str "Various Examples" ]
        yield div [ Style [ Display DisplayOptions.Table; Margin "auto" ]] [
            yield button [ OnClick (fun _ ->  ToDragAndDrop |> dispatch)] [ str "Drag And Drop" ]
            yield button [ OnClick (fun _ ->  ToSingleListDemo |> dispatch)] [ str "Single List Demo" ]
            yield button [ OnClick (fun _ ->  ToMultiListDemo |> dispatch)] [ str "Multi List Demo" ]
            yield button [ OnClick (fun _ ->  ToHandlesDemo |> dispatch)] [ str "Handles Demo" ]
            yield button [ OnClick (fun _ ->  ToHorizontalDemo |> dispatch)] [ str "Horizontal Demo"]
            yield button [ OnClick (fun _ ->  ToSliding |> dispatch)] [ str "Sliding" ]
        ]
        match model.Page with
        //| ListSorting ls -> yield ListSorting.view ls (fun x -> x |> ListSortingMsg |> dispatch)
        | RateLimiting rl ->yield RateLimiting.view rl (fun x -> x |> RateLimitingMsg |> dispatch)
        | DragAndDrop2 dnd -> yield Pages.CollectionDragAndDrop2.view dnd (fun x -> x |> DragAndDrop2Msg |> dispatch )
        | SingleListDemo dnd -> yield Pages.SingleListDemo.view dnd (fun x -> x |> SingleListDemoMsg |> dispatch )
        | MultiListDemo dnd -> yield Pages.MultiListDemo.view dnd (fun x -> x |> MultiListDemoMsg |> dispatch )
        | HandlesDemo dnd -> yield Pages.HandlesDemo.view dnd (fun x -> x |> HandlesDemoMsg |> dispatch )
        | HorizontalDemo dnd -> yield Pages.HorizontalDemo.view dnd (fun x -> x |> HorizontalDemoMsg |> dispatch )
        | Sliding m -> yield Pages.Sliding.React.view m (fun x -> SlidingMsg x |> dispatch)
    ]


let update msg model =
    match msg, model.Page with
    | RateLimitingMsg msg, RateLimiting mdl ->
        let mdl, cmd = RateLimiting.update msg mdl
        { Page = RateLimiting mdl}, Cmd.map RateLimitingMsg cmd
    | DragAndDrop2Msg msg, DragAndDrop2 mdl ->
        let mdl, cmd = Pages.CollectionDragAndDrop2.update msg mdl
        { Page = DragAndDrop2 mdl }, Cmd.map DragAndDrop2Msg cmd
    | SlidingMsg msg , Sliding mdl ->
        printfn "root msg is %A" msg
        let mdl, cmd = Pages.Sliding.React.update msg mdl
        { Page = Sliding mdl }, Cmd.map SlidingMsg cmd
    | ToDragAndDrop, _ ->
        let mdl = Pages.CollectionDragAndDrop2.init()
        { Page = DragAndDrop2 mdl}, Cmd.none
    | ToSliding, _ ->
        let mdl = Pages.Sliding.React.Model.Init()
        { Page = Sliding mdl }, Cmd.ofMsg (SlidingMsg (Pages.Sliding.React.Loading))
    | ToSingleListDemo, _ ->
        let mdl = Pages.SingleListDemo.init()
        { Page = SingleListDemo mdl}, Cmd.ofMsg (SingleListDemoMsg Pages.SingleListDemo.Init)
    | SingleListDemoMsg msg, SingleListDemo mdl ->
        let mdl, cmd = Pages.SingleListDemo.update msg mdl
        { Page = SingleListDemo mdl}, Cmd.map (SingleListDemoMsg) cmd
    | ToMultiListDemo, _ ->
        let mdl = Pages.MultiListDemo.init()
        { Page = MultiListDemo mdl}, Cmd.ofMsg (MultiListDemoMsg Pages.MultiListDemo.Init)
    | MultiListDemoMsg msg, MultiListDemo mdl ->
        let mdl, cmd = Pages.MultiListDemo.update msg mdl
        { Page = MultiListDemo mdl}, Cmd.map (MultiListDemoMsg) cmd
    | ToHandlesDemo, _ ->
        let mdl = Pages.HandlesDemo.init()
        { Page = HandlesDemo mdl}, Cmd.ofMsg (HandlesDemoMsg Pages.HandlesDemo.Init)
    | HandlesDemoMsg msg, HandlesDemo mdl ->
        let mdl, cmd = Pages.HandlesDemo.update msg mdl
        { Page = HandlesDemo mdl}, Cmd.map (HandlesDemoMsg) cmd
    | ToHorizontalDemo, _ ->
        let mdl = Pages.HorizontalDemo.init()
        { Page = HorizontalDemo mdl}, Cmd.ofMsg (HorizontalDemoMsg Pages.HorizontalDemo.Init)
    | HorizontalDemoMsg msg, HorizontalDemo mdl ->
        let mdl, cmd = Pages.HorizontalDemo.update msg mdl
        { Page = HorizontalDemo mdl}, Cmd.map (HorizontalDemoMsg) cmd
    | _, _ -> model, Cmd.none

Program.mkProgram
    init
    update
    view
|> Program.withReactBatched "app"
|> Program.run
