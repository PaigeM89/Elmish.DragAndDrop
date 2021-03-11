module App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open App


type Page =
| ListSorting of model : App.Pages.ListSorting.Model
| Bucket of model : App.Pages.Bucket.Model

type Model = {
    Page : Page
}

let init() =
    // let m = App.Pages.ListSorting.Model.Init()
    // { Page = ListSorting m }, Cmd.none
    let m = App.Pages.Bucket.Model.Init()
    { Page = Bucket m }, Cmd.none    

type Msg =
| ListSortingMsg of App.Pages.ListSorting.Msg
| DisplayListSorting
| BucketMsg of App.Pages.Bucket.Msg
| DisplayBucket

let view model (dispatch : Msg -> unit) =
    div [] [
        yield h2 [Style [ CSSProp.TextAlign TextAlignOptions.Center ] ] [ str "Drag And Drop Examples" ]
        match model.Page with
        | ListSorting ls -> yield Pages.ListSorting.view ls (fun x -> x |> ListSortingMsg |> dispatch)
        | Bucket b -> yield Pages.Bucket.view b (fun x -> x |> BucketMsg |> dispatch)
    ]


let update msg model =
    match msg, model.Page with
    | ListSortingMsg m, ListSorting mdl ->
        let mdl, cmd = Pages.ListSorting.update m mdl
        { Page = ListSorting mdl }, Cmd.map ListSortingMsg cmd
    | DisplayListSorting, _ ->
        { Page = ListSorting (Pages.ListSorting.Model.Init()) }, Cmd.none
    | BucketMsg msg, Bucket mdl ->
        let mdl, cmd = Pages.Bucket.update msg mdl
        { Page = Bucket mdl }, Cmd.map BucketMsg cmd
    | DisplayBucket, _ ->
        { Page = Bucket (Pages.Bucket.Model.Init()) }, Cmd.none
    | _, _ -> model, Cmd.none

Program.mkProgram
    init
    update
    view
|> Program.withReactBatched "app"
|> Program.run
