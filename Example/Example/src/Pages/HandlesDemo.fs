namespace Pages

module HandlesDemo =
  open Feliz
  open Fable.React
  open Fable.React.Props
  open Browser.Dom
  open Browser.Types
  open Elmish
  open Elmish.React
  open Elmish.DragAndDrop
  // open Elmish.Handles


  type ContentKey = string
  type ContentValue = { UserInput : string; Name : string}

  type Model = {
    DragAndDrop : DragAndDrop.Types.Model
    ContentMap : Map<ContentKey, ContentValue>
  }

  let init() = 
    {
      DragAndDrop = DragAndDrop.Types.Model.Empty()
      ContentMap = Map.empty
    }

  type Msg =
  | Init
  | DndMsg of DragAndDrop.Types.Msg
  | InputChange of elementId : string * newValue : string

  let mappedMsg msg = DndMsg msg

  let createDraggableTemplate className = {
    DragAndDrop.Types.DragAndDropConfig.Empty() with
      DraggedElementStyles = Some [
          MarginLeft -130.
          MarginTop -50.
          Opacity 0.8
          Position PositionOptions.Fixed
          Cursor "grabbing"
      ]
      DraggedElementProperties = Some [
        ClassName (className + " li-dragged")
      ]
      DraggableElementStyles = Some []
      HoverPreviewElementStyles = Some [
        Opacity 0.2
      ]
      // DefaultClass = Some className
  }


  let inputValueLookup model elementId =
    Map.tryFind elementId model.ContentMap
    |> Option.defaultValue { UserInput = ""; Name = "Unknown input"}


  let createGenerators dndModel (rootElementId : string) inputId dispatch value =
    let content = [
      DragHandle.dragHandle dndModel rootElementId (mappedMsg >> dispatch) (
        ElementGenerator.Create (sprintf "%s-handle" rootElementId) [ Cursor "grab" ] [] [h3 [] [ str (value.Name)]]
      )
      input [
        Id inputId
        DefaultValue value.UserInput
        OnChange (fun ev ->
          let v = ev.Value 
          InputChange (inputId, v) |> (dispatch))
      ]
    ]
    ElementGenerator.Create rootElementId [] [] content
    // [
    //   DragHandle.dragHandle msging [ Style [ Cursor "grab" ]] [ h3 [] [ str (value.Name)] ]
    //   input [
    //     Id inputId
    //     DefaultValue value.UserInput
    //     OnChange (fun ev -> 
    //       let v = ev.Value 
    //       InputChange (inputId, v) |> (dispatch))
    //   ]
    // ]

  let view model (dispatch : Msg -> unit) =
    let template = createDraggableTemplate "li-content"
    let dropAreaProps =
      [
        (ClassName "container") :> IHTMLProp
      ] // |> AreaProps
    let dropAreaContent =
      model.DragAndDrop.ElementIds()
      |> List.map (fun li ->
        li
        |> List.map (fun (inputId) ->
          let rootElementId = sprintf "%s-root" inputId
          let content =
            inputValueLookup model inputId
            |> createGenerators model.DragAndDrop rootElementId inputId dispatch

          rootElementId, content
        )
        |> DropArea.dropArea model.DragAndDrop (mappedMsg >> dispatch) template dropAreaProps
      )
    div [
      //ClassName "backdrop"
      Style [
        Background "#0066ff"
        Width "100%"
      ]
    ][
      div [ ClassName "wrapper" ] dropAreaContent
    ]

  let update msg model =
    match msg with
    | Init ->
      printfn "in init"
      let content = [
        for i in 1..7 do 
          yield (sprintf "input-%i" i), { UserInput = ""; Name = sprintf "Input %i" i }
      ]
      let elementIds = content |> List.map fst
      let m = content |> Map.ofList
      let dndModel = DragAndDrop.Types.Model.createWithItems elementIds
      { model with DragAndDrop = dndModel; ContentMap = m }, Cmd.none
    | DndMsg msg ->
      let dndModel, cmd = DragAndDrop.Update.update msg model.DragAndDrop
      { model with DragAndDrop = dndModel }, Cmd.map DndMsg cmd
    | InputChange (elementId, newValue) ->
      printfn "input change: %A value %A" elementId newValue
      match Map.tryFind elementId model.ContentMap with
      | Some oldContent ->
        let newValue = { oldContent with UserInput = newValue}
        let m = Map.add elementId newValue model.ContentMap
        { model with ContentMap = m }, Cmd.none
      | None ->
        let newValue = { UserInput = newValue; Name = "" }
        let m = Map.add elementId newValue model.ContentMap
        { model with ContentMap = m }, Cmd.none