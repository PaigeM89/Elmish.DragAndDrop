module App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props


type Page =
| ListSorting of model : ListSorting.Model
| RateLimiting of model :  RateLimiting.Model
| DragAndDrop2 of model : Pages.CollectionDragAndDrop2.Model
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
| DragAndDrop2Msg of Pages.CollectionDragAndDrop2.Msg
// | BucketMsg of App.Pages.Bucket.Msg
// | DisplayBucket

let view model (dispatch : Msg -> unit) =
    div [] [
        yield h2 [Style [ CSSProp.TextAlign TextAlignOptions.Center ] ] [ str "Drag And Drop Examples" ]
        match model.Page with
        | ListSorting ls -> yield ListSorting.view ls (fun x -> x |> ListSortingMsg |> dispatch)
        | RateLimiting rl ->yield RateLimiting.view rl (fun x -> x |> RateLimitingMsg |> dispatch)
        | DragAndDrop2 dnd -> yield Pages.CollectionDragAndDrop2.view dnd (fun x -> x |> DragAndDrop2Msg |> dispatch )
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
    // | BucketMsg msg, Bucket mdl ->
    //     let mdl, cmd = Pages.Bucket.update msg mdl
    //     { Page = Bucket mdl }, Cmd.map BucketMsg cmd
    // | DisplayBucket, _ ->
    //     { Page = Bucket (Pages.Bucket.Model.Init()) }, Cmd.none
    | _, _ -> model, Cmd.none

Program.mkProgram
    init
    update
    view
|> Program.withReactBatched "app"
|> Program.run
