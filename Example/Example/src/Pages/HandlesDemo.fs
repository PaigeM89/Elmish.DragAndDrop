namespace Pages

module HandlesDemo =
  open Feliz
  open Fable.React
  open Fable.React.Props
  open Browser.Dom
  open Browser.Types
  open Elmish
  open Elmish.React
  open Elmish.Handles


  type ContentKey = string
  type ContentValue = { UserInput : string; Name : string}

  type Model = {
    DragAndDrop : Handles.Model
    //Content : (string * string) list
    ContentMap : Map<ContentKey, ContentValue>
    //InputValueMap: Map<string, string>
  }

  let init() = 
    {
      DragAndDrop = Handles.empty()
      //Content = []
      ContentMap = Map.empty
      //InputValueMap = Map.empty
    }

  type Msg =
  | Init
  | DndMsg of Handles.Msg
  | InputChange of elementId : string * newValue : string

  let mappedMsg msg = DndMsg msg

  let createDraggableTemplate className = {
    Handles.DraggableTemplate.Empty() with
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
      DraggableElementStyles = Some [
//        Cursor "grab"
      ]
      HoverPreviewElementStyles = Some [
        Opacity 0.2
        // PointerEvents "None"
      ]
      // SlidingElementStyles = Some [
      //   CSSProp.TransitionDuration 1.0
      //   PointerEvents "None"
      //   Position PositionOptions.Fixed
      // ]
      DefaultClass = Some className
      // HandlesStyles = Some [
      //   Cursor "grab"
      //   // Left "35px"
      //   // Top "50px"
      //   // CSSProp.Float FloatOptions.Left
      //   // Position PositionOptions.Relative
      //   Left 0
      //   Top 0
      //   Position PositionOptions.Absolute
      //   BackgroundColor "3fd4d8"
      // ]
      // Handle = Html.em [ str "Grab me!" ] |> Some
  }


  let inputValueLookup model elementId =
    Map.tryFind elementId model.ContentMap
    |> Option.defaultValue { UserInput = ""; Name = "Unknown input"}


  let genContent msging inputId dispatch value =
    [
      DragHandle.dragHandle msging [ Style [ Cursor "grab" ]] [ h3 [] [ str (value.Name)] ]
      input [
        Id inputId
        DefaultValue value.UserInput
        OnChange (fun ev -> 
          let v = ev.Value 
          InputChange (inputId, v) |> (dispatch))
      ]
    ]

  let view model (dispatch : Msg -> unit) =
    let template = createDraggableTemplate "li-content"
    let dropAreaProps =
      [
        (ClassName "container") :> IHTMLProp
      ] |> AreaProps
    let dropAreaContent =
      model.DragAndDrop.ElementIds()
      |> List.map (fun li ->
        li
        |> List.map (fun (id) ->
          let msging = Messaging.Create model.DragAndDrop id (mappedMsg >> dispatch)
          let content = inputValueLookup model id |> genContent msging id dispatch

          msging, content
        )
        |> DropArea.dropArea model.DragAndDrop (mappedMsg >> dispatch) [dropAreaProps] template
      )
    div [
      ClassName "backdrop"
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
      let dndModel = Handles.ModelFuncs.createWithItems elementIds
      { model with DragAndDrop = dndModel; ContentMap = m }, Cmd.none
    | DndMsg msg ->
      let dndModel, cmd = Handles.update msg model.DragAndDrop
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