namespace Pages

open System

(*
  This demo shows how to create a drag-and-drop setup with table rows.

  TODO: Fix the numeric & text input boxes
*)

module TableDemo =
  open Fable.React
  open Fable.React.Props
  open Elmish
  open Elmish.DragAndDrop2

  type ContentValue = {
    ContentId : Guid
    SomeInt : int
    SomeString : string
    IsChecked : bool
  } with
    static member Empty() = {
      ContentId = Guid.NewGuid()
      SomeInt = 0
      SomeString = ""
      IsChecked = false
    }

  type Model = {
    DragAndDrop : DragAndDropModel
    ContentMap : Map<string, ContentValue>
  }

  let init() = {
    DragAndDrop = DragAndDropModel.Empty()
    ContentMap = Map.empty
  }

  let initWithSampleData() =
    let data = [
      { ContentValue.Empty() with SomeInt = 1; SomeString = "hello world"; IsChecked = true}
      { ContentValue.Empty() with SomeInt = 3; SomeString = "foo"; IsChecked = true}
      { ContentValue.Empty() with SomeInt = 5; SomeString = "bar"; IsChecked = false}
      { ContentValue.Empty() with SomeInt = 7; SomeString = "bax"; IsChecked = false}
    ]
    let dnd = DragAndDropModel.createWithItems (data |> List.map (fun x -> x.ContentId |> string))
    let m = data |> List.map(fun x -> (string x.ContentId), x) |> Map.ofList
    {
      DragAndDrop = dnd
      ContentMap = m
    }

  type Msg =
  | Init
  | InitWithSampleData
  | DndMsg of DragAndDropMsg
  | Checkbox of contentId : Guid * status : bool
  | AddRow
  | DeleteRow of rowId : Guid

  let dndMsg msg = DndMsg msg

  let dragAndDropConfig = {
    DragAndDropConfig.Empty() with
      DraggedElementStyles = Some [
          MarginLeft -130.
          MarginTop -50.
          Opacity 0.8
          Position PositionOptions.Fixed
          Cursor "grabbing"
          Background "#00ffff"
      ]
      HoverPreviewElementStyles = Some [
        Opacity 0.2
      ]
  }

  module View =

    let numericInput value =
      input [
        DefaultValue value
        OnChange (fun ev -> ())
      ]

    let createTableRow model dispatch index (cv : ContentValue)=
      let rowId = cv.ContentId |> string
      let handleStyles = if model.DragAndDrop.Moving.IsSome then [] else [ Cursor "grab" ]

      // note that the content is the whole collection of table cells, without any wrapping tag.
      let content = [
          td [] [
            DragHandle.Handle
              model.DragAndDrop
              rowId
              (dndMsg >> dispatch)
              div
              [ Id (rowId + "-handle"); Style handleStyles]
              [
                h2 [] [ str (string index) ]
              ]
          ]
          td [] [
            numericInput cv.SomeInt
          ]
          td [] [
            input [
              DefaultValue cv.SomeString
              OnChange (fun ev -> ())
            ]
          ]
          td [] [
            input [
              Type "checkbox"
              Checked cv.IsChecked
              OnChange (fun ev -> (cv.ContentId, ev.Checked) |> Checkbox |> dispatch)
            ]
            label [] [ str "Checkbox label" ]
          ]
          td [] [
            button [
              OnClick (fun _ -> DeleteRow cv.ContentId |> dispatch)
            ] [
              str "Delete"
            ]
          ]
      ]
      Draggable.InnerHandle
        model.DragAndDrop
        dragAndDropConfig
        (dndMsg >> dispatch)
        rowId
        tr
        []
        [ Id rowId ]
        content

    let addRowButton dispatch = 
      button [
        Style [
          MarginLeft "auto"
          MarginRight "auto"
          Display DisplayOptions.Block
          MarginTop 10
          MarginBottom 10
        ]
        OnClick (fun _ -> AddRow |> dispatch)
      ] [
        str "Add Row"
      ]

    let sampleDataInitButton dispatch =
      button [
        Style [
          MarginLeft "auto"
          MarginRight "auto"
          Display DisplayOptions.Block
          MarginTop 10
          MarginBottom 10
        ]
        OnClick (fun _ -> InitWithSampleData |> dispatch)
      ] [
        str "Initialize Sample Data"
      ]

    let view model dispatch =
      let tableHeaders = 
        thead [] [
          tr [] [
            th [] [ str "#"]
            th [] [ str "Some Number" ]
            th [] [ str "Some String" ]
            th [] [ str "Some Checkbox" ]
            th [] [] // empty for delete button
          ]
        ]

      let rows =
        model.DragAndDrop.ElementIds()
        |> List.tryHead |> Option.defaultValue []
        |> List.mapi (fun index id ->
          let cv = model.ContentMap |> Map.find id
          createTableRow model dispatch index cv
        )
        |> List.concat
      
      let table = 
        table [
          Style [
            MarginLeft "auto"
            MarginRight "auto"
            Background "#33adff"
            PaddingTop 10
            MarginTop 20
          ]
        ] [
          tableHeaders
          DropArea.DropArea 
            model.DragAndDrop
            dragAndDropConfig
            (MouseEventHandlers.Empty())
            (dndMsg >> dispatch)
            "drop-area"
            tbody
            []
            rows
        ]

      let props : IHTMLProp list = [
        Style [
          //Background "#43d8dc"
          Background "#0066ff"
          PaddingTop 10
          PaddingBottom 100
        ]
      ]
      let content = [
        addRowButton dispatch
        sampleDataInitButton dispatch
        table
      ]

      DragDropContext.Context model.DragAndDrop (DndMsg >> dispatch) div props content
  
  let addContent model cv =
    let dict = model.ContentMap |> Map.add (string cv.ContentId) cv
    let dnd = DragAndDropModel.insertNewItemAtHead 0 (string cv.ContentId) model.DragAndDrop
    { model with ContentMap = dict; DragAndDrop = dnd }

  let removeContent model id =
    let m = model.ContentMap |> Map.remove (string id)
    let dnd = DragAndDropModel.removeItem (string id) model.DragAndDrop
    { model with ContentMap = m; DragAndDrop = dnd }

  let update msg model =
    match msg with
    | Init ->
      model, Cmd.none
    | InitWithSampleData ->
      (initWithSampleData()), Cmd.none
    | DndMsg msg ->
      let dndModel, cmd = dragAndDropUpdate msg model.DragAndDrop
      { model with DragAndDrop = dndModel }, Cmd.map DndMsg cmd
    | AddRow ->
      let row = ContentValue.Empty()
      let model = addContent model row
      model, Cmd.none
    | DeleteRow rowId ->
      let model = removeContent model rowId
      model, Cmd.none
    | Checkbox(contentId, status) ->
      let contentIdStr = string contentId
      match Map.tryFind contentIdStr model.ContentMap with
      | Some content ->
        let content = { content with IsChecked = status }
        let m = Map.add contentIdStr content model.ContentMap
        { model with ContentMap = m }, Cmd.none
      | None ->
        model, Cmd.none
