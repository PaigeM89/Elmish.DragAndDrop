namespace Pages

open System

module TableDemo =
  open Fable.React
  open Fable.React.Props
  open Elmish
  open Elmish.DragAndDrop

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

  type Msg =
  | Init
  | DndMsg of DragAndDropMsg
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
      ]

    let createTableRow model dispatch index (cv : ContentValue)=
      let rowId = cv.ContentId |> string
      let rootElementId = rowId
      let handleStyles = if model.DragAndDrop.Moving.IsSome then [] else [ Cursor "grab" ]

      let content = [
          td [] [
            DragHandle.Rendered model.DragAndDrop rootElementId (DndMsg >> dispatch) (
              ElementGenerator.Create (rowId + "-handle") handleStyles [] [
                h2 [] [ str (string index)]
              ]
            )
          ]
          td [] [
            numericInput cv.SomeInt
          ]
          td [] [
            input [
              DefaultValue cv.SomeString
            ]
          ]
          td [] [
            input [
              InputMode "checkbox"
              Checked cv.IsChecked
              OnChange (fun ev -> ())
            ]
          ]
      ]
      let gen = 
        ElementGenerator.Create rootElementId [] [Id rowId ] content
        |> ElementGenerator.setTag tr
      rowId, gen

    let addRowButton dispatch = 
      button [
        Style [
          MarginLeft "auto"
          MarginRight "auto"
        ]
        OnClick (fun _ -> AddRow |> dispatch)
      ] [
        str "Add Row"
      ]

    let view model dispatch =
      let tableHeaders = 
        thead [] [
          tr [] [
            th [] [ str "#"] // blank for drag handle
            th [] [ str "Some Number" ]
            th [] [ str "Some String" ]
            th [] [ str "Some Checkbox" ]
          ]
        ]

      let rows =
        model.DragAndDrop.ElementIds()
        |> List.tryHead |> Option.defaultValue []
        |> List.mapi (fun index id ->
          let cv = model.ContentMap |> Map.find id
          createTableRow model dispatch index cv
        )
      
      let table = 
        table [
          Style [
            MarginLeft "auto"
            MarginRight "auto"
          ]
        ] [
          tableHeaders
          
          DropArea.fromGeneratorsWithTag
            model.DragAndDrop
            (DndMsg >> dispatch)
            dragAndDropConfig
            []
            rows
            tbody
          
        ]
      div [
        Style [
          Background "#43d8dc"
        ]
      ] [
        addRowButton dispatch
        table
      ]
  
  let addContent model cv =
    let dict = model.ContentMap |> Map.add (string cv.ContentId) cv
    let dnd = DragAndDropModel.insertNewItemAtHead 0 (string cv.ContentId) model.DragAndDrop
    { model with ContentMap = dict; DragAndDrop = dnd }

  let update msg model =
    match msg with
    | Init ->
      model, Cmd.none
    | DndMsg msg ->
      let dndModel = dragAndDropUpdate msg model.DragAndDrop
      { model with DragAndDrop = dndModel }, Cmd.none
    | AddRow ->
      let row = ContentValue.Empty()
      let model = addContent model row
      model, Cmd.none
    | DeleteRow rowId ->
      model, Cmd.none
