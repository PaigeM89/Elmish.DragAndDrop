module App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props


type Page =
| ListSorting of model : ListSorting.Model
| RateLimiting of model :  RateLimiting.Model
| DragAndDrop2 of model : Pages.CollectionDragAndDrop2.Model
| DragAndDrop3 of model : Pages.CollectionDragAndDrop3.Model
| Sliding of model : Pages.Sliding.React.Model
//| Bucket of model : App.Pages.Bucket.Model

type Model = {
    Page : Page
}

let init() =
    // let m = App.Pages.ListSorting.Model.Init()
    // { Page = ListSorting m }, Cmd.none
    // let m = ListSorting.Model.Init()
    // { Page = ListSorting m }, Cmd.none
    // let m = RateLimiting.init()
    // { Page = RateLimiting m}, Cmd.none

    let m = Pages.CollectionDragAndDrop2.init()
    { Page = DragAndDrop2 m}, Cmd.none

type Msg =
| ListSortingMsg of ListSorting.Msg
| DisplayListSorting
| RateLimitingMsg of RateLimiting.Msg
| ToDragAndDrop
| ToDragAndDrop3
| DragAndDrop2Msg of Pages.CollectionDragAndDrop2.Msg
| DragAndDrop3Msg of Pages.CollectionDragAndDrop3.Msg
| ToSliding
| SlidingMsg of Pages.Sliding.React.Msg
// | BucketMsg of App.Pages.Bucket.Msg
// | DisplayBucket

[<Feliz.ReactComponent>]
let Root(m : Model) =
    Pages.Sliding.React.view m (fun _ ->())

let view model (dispatch : Msg -> unit) =
    div [] [
        yield h2 [Style [ CSSProp.TextAlign TextAlignOptions.Center ] ] [ str "Various Examples" ]
        yield div [ Style [ Display DisplayOptions.Table; Margin "auto" ]] [
            yield button [ OnClick (fun _ ->  ToDragAndDrop |> dispatch)] [ str "Drag And Drop" ]
            yield button [ OnClick (fun _ ->  ToDragAndDrop3 |> dispatch)] [ str "Drag And Drop 3" ]
            yield button [ OnClick (fun _ ->  ToSliding |> dispatch)] [ str "Sliding" ]
        ]
        match model.Page with
        | ListSorting ls -> yield ListSorting.view ls (fun x -> x |> ListSortingMsg |> dispatch)
        | RateLimiting rl ->yield RateLimiting.view rl (fun x -> x |> RateLimitingMsg |> dispatch)
        | DragAndDrop2 dnd -> yield Pages.CollectionDragAndDrop2.view dnd (fun x -> x |> DragAndDrop2Msg |> dispatch )
        | DragAndDrop3 dnd -> yield Pages.CollectionDragAndDrop3.view dnd (fun x -> x |> DragAndDrop3Msg |> dispatch )
        | Sliding m -> yield Pages.Sliding.React.view m (fun x -> SlidingMsg x |> dispatch)
        //| Bucket b -> yield Pages.Bucket.view b (fun x -> x |> BucketMsg |> dispatch)
    ]


let update msg model =
    match msg, model.Page with
    | ListSortingMsg m, ListSorting mdl ->
        let mdl, cmd = ListSorting.update m mdl
        { Page = ListSorting mdl }, Cmd.map ListSortingMsg cmd
    | DisplayListSorting, _ ->
        { Page = ListSorting (ListSorting.Model.Init()) }, Cmd.none
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
    // | BucketMsg msg, Bucket mdl ->
    //     let mdl, cmd = Pages.Bucket.update msg mdl
    //     { Page = Bucket mdl }, Cmd.map BucketMsg cmd
    // | DisplayBucket, _ ->
    //     { Page = Bucket (Pages.Bucket.Model.Init()) }, Cmd.none
    | ToDragAndDrop, _ ->
        let mdl = Pages.CollectionDragAndDrop2.init()
        { Page = DragAndDrop2 mdl}, Cmd.none
    | ToSliding, _ ->
        let mdl = Pages.Sliding.React.Model.Init()
        { Page = Sliding mdl }, Cmd.ofMsg (SlidingMsg (Pages.Sliding.React.Loading))
    | ToDragAndDrop3, _ ->
        printfn "to drag and drop 3"
        let mdl = Pages.CollectionDragAndDrop3.init()
        { Page = DragAndDrop3 mdl}, Cmd.ofMsg (DragAndDrop3Msg Pages.CollectionDragAndDrop3.Init)
    | DragAndDrop3Msg msg, DragAndDrop3 mdl ->
        let mdl, cmd = Pages.CollectionDragAndDrop3.update msg mdl
        { Page = DragAndDrop3 mdl}, Cmd.map (DragAndDrop3Msg) cmd
    | _, _ -> model, Cmd.none

Program.mkProgram
    init
    update
    view
|> Program.withReactBatched "app"
|> Program.run
