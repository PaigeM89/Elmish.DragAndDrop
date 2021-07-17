module App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props

type Page =
| SingleListDemo of model : Pages.SingleListDemo.Model
| MultiListDemo of model : Pages.MultiListDemo.Model
| HandlesDemo of model : Pages.HandlesDemo.Model
| TableDemo of model : Pages.TableDemo.Model
| DropToDeleteDemo of model : Pages.DropToDeleteDemo.Model
| MultipleDragTypesDemo of model : Pages.MultipleDragTypesDemo.Model
| MultipleModelsDemo of model : Pages.MultipleModelsDemo.Model
// | HorizontalDemo of model : Pages.HorizontalDemo.Model


type Model = {
    Page : Page
}

let init() =
    let m = Pages.SingleListDemo.init()
    { Page = SingleListDemo m}, Cmd.none

type Msg =
| SingleListDemoMsg of Pages.SingleListDemo.Msg
| MultiListDemoMsg of Pages.MultiListDemo.Msg
// | HorizontalDemoMsg of Pages.HorizontalDemo.Msg
| HandlesDemoMsg of Pages.HandlesDemo.Msg
| TableDemoMsg of Pages.TableDemo.Msg
| DropToDeleteDemoMsg of Pages.DropToDeleteDemo.Msg
| MultipleDragtypesDemoMsg of Pages.MultipleDragTypesDemo.Msg
| MultipleModelsDemoMsg of Pages.MultipleModelsDemo.Msg
| ToSingleListDemo
| ToMultiListDemo
| ToHandlesDemo
| ToTableDemo
| ToDropToDeleteDemo
| ToMultipleDragTypesDemo
| ToMultipleModelsDemo
// | ToHorizontalDemo

let view model (dispatch : Msg -> unit) =
    div [] [
        yield h2 [Style [ CSSProp.TextAlign TextAlignOptions.Center ] ] [ str "Various Examples" ]
        yield div [ Style [ Display DisplayOptions.Table; Margin "auto" ]] [
            yield button [ OnClick (fun _ ->  ToSingleListDemo |> dispatch)] [ str "Single List Demo" ]
            yield button [ OnClick (fun _ ->  ToMultiListDemo |> dispatch)] [ str "Multi List Demo" ]
            yield button [ OnClick (fun _ ->  ToHandlesDemo |> dispatch)] [ str "Handles Demo" ]
            yield button [ OnClick (fun _ -> ToTableDemo |> dispatch)] [ str "Table Demo" ]
            yield button [ OnClick (fun _ -> ToDropToDeleteDemo |> dispatch)] [ str "Drop To Delete Demo" ]
            yield button [ OnClick (fun _ -> ToMultipleDragTypesDemo |> dispatch)] [ str "Multiple Drag Types Demo" ]
            yield button [ OnClick (fun _ -> ToMultipleModelsDemo |> dispatch )] [ str "Multiple Drag And Drop Models Demo" ]
            // yield button [ OnClick (fun _ ->  ToHorizontalDemo |> dispatch)] [ str "Horizontal Demo"]
        ]
        match model.Page with
        | SingleListDemo dnd -> yield Pages.SingleListDemo.view dnd (fun x -> x |> SingleListDemoMsg |> dispatch )
        | MultiListDemo dnd -> yield Pages.MultiListDemo.view dnd (fun x -> x |> MultiListDemoMsg |> dispatch )
        | HandlesDemo dnd -> yield Pages.HandlesDemo.view dnd (fun x -> x |> HandlesDemoMsg |> dispatch )
        | TableDemo dnd -> yield Pages.TableDemo.View.view dnd (fun x -> x |> TableDemoMsg |> dispatch )
        | DropToDeleteDemo dnd -> yield Pages.DropToDeleteDemo.view dnd (fun x -> x |> DropToDeleteDemoMsg |> dispatch )
        | MultipleDragTypesDemo dnd -> yield Pages.MultipleDragTypesDemo.view dnd (MultipleDragtypesDemoMsg >> dispatch)
        | MultipleModelsDemo dnd -> yield Pages.MultipleModelsDemo.view dnd (MultipleModelsDemoMsg >> dispatch)
        // | HorizontalDemo dnd -> yield Pages.HorizontalDemo.view dnd (fun x -> x |> HorizontalDemoMsg |> dispatch )
    ]


let update msg model =
    match msg, model.Page with
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
    | ToTableDemo, _ ->
      let mdl = Pages.TableDemo.init()
      { Page = TableDemo mdl}, Cmd.ofMsg (TableDemoMsg Pages.TableDemo.Init)
    | TableDemoMsg msg, TableDemo mdl ->
      let mdl, cmd = Pages.TableDemo.update msg mdl
      { Page = TableDemo mdl}, Cmd.map (TableDemoMsg) cmd
    | ToDropToDeleteDemo, _ ->
      let mdl = Pages.DropToDeleteDemo.init()
      { Page = DropToDeleteDemo mdl}, Cmd.ofMsg (DropToDeleteDemoMsg Pages.DropToDeleteDemo.Init)
    | DropToDeleteDemoMsg msg, DropToDeleteDemo mdl ->
      let mdl, cmd = Pages.DropToDeleteDemo.update msg mdl
      { Page = DropToDeleteDemo mdl}, Cmd.map (DropToDeleteDemoMsg) cmd
    | MultipleDragtypesDemoMsg msg, MultipleDragTypesDemo mdl ->
      let mdl, cmd = Pages.MultipleDragTypesDemo.update msg mdl
      { Page = MultipleDragTypesDemo mdl }, Cmd.map (MultipleDragtypesDemoMsg) cmd
    | ToMultipleDragTypesDemo, _ ->
      let mdl = Pages.MultipleDragTypesDemo.Model.Init()
      { Page = MultipleDragTypesDemo mdl }, Cmd.ofMsg (MultipleDragtypesDemoMsg Pages.MultipleDragTypesDemo.Init)
    | MultipleModelsDemoMsg msg, MultipleModelsDemo mdl ->
      let mdl, cmd = Pages.MultipleModelsDemo.update msg mdl
      { Page = MultipleModelsDemo mdl }, Cmd.map (MultipleModelsDemoMsg) cmd
    | ToMultipleModelsDemo, _ ->
      let mdl = Pages.MultipleModelsDemo.Model.Init()
      { Page = MultipleModelsDemo mdl }, Cmd.ofMsg (MultipleModelsDemoMsg Pages.MultipleModelsDemo.Init)
    // | ToHorizontalDemo, _ ->
    //     let mdl = Pages.HorizontalDemo.init()
    //     { Page = HorizontalDemo mdl}, Cmd.ofMsg (HorizontalDemoMsg Pages.HorizontalDemo.Init)
    // | HorizontalDemoMsg msg, HorizontalDemo mdl ->
    //     let mdl, cmd = Pages.HorizontalDemo.update msg mdl
    //     { Page = HorizontalDemo mdl}, Cmd.map (HorizontalDemoMsg) cmd
    | _, _ -> model, Cmd.none

Program.mkProgram
    init
    update
    view
|> Program.withReactBatched "app"
|> Program.run
