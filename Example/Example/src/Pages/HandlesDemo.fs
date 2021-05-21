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

  let dragAndDropConfig = {
    DragAndDrop.Types.DragAndDropConfig.Empty() with
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

  let defaultStyles = [
    MarginTop 20
    MarginLeft 20
    MarginRight 20
    MarginBottom 20
    TextAlign TextAlignOptions.Center
    Background "#33ccff"
    Padding 10
    Border "1px solid black"
    Width 300
    MinHeight 50
  ]

  let inputValueLookup model elementId =
    Map.tryFind elementId model.ContentMap
    |> Option.defaultValue { UserInput = ""; Name = "Unknown input"}


  let createGenerators dndModel (rootElementId : string) dispatch value =
    let handleStyles = if dndModel.Moving.IsSome then [] else [ Cursor "grab" ]
    let content = [
      DragHandle.dragHandle dndModel rootElementId (mappedMsg >> dispatch) (
        ElementGenerator.Create (sprintf "%s-handle" rootElementId) handleStyles [] [h3 [] [ str (value.Name)]]
      )
      input [
        DefaultValue value.UserInput
        OnChange (fun ev ->
          let v = ev.Value 
          // we track the current state of user input by the root draggable's ID, not the input id (which we dont set)
          InputChange (rootElementId, v) |> (dispatch))
      ]
    ]
    ElementGenerator.Create rootElementId defaultStyles [] content

  let generateRootId id = sprintf "%s-root" id

  let view model (dispatch : Msg -> unit) =
    let dropAreaProps =
      [
        (ClassName "container") :> IHTMLProp
      ]
    let dropAreaContent =
      model.DragAndDrop.ElementIds()
      |> List.map (fun li ->
        li
        |> List.map (fun (rootElementId) ->
          let content =
            inputValueLookup model rootElementId
            |> createGenerators model.DragAndDrop rootElementId dispatch
          rootElementId, content
        )
        |> DropArea.dropArea model.DragAndDrop (mappedMsg >> dispatch) dragAndDropConfig dropAreaProps
      )
    div [
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
          yield (sprintf "input-%i" i |> generateRootId), { UserInput = ""; Name = sprintf "Input %i" i }
      ]
      let elementIds = content |> List.map (fst)
      let m = content |> Map.ofList
      let dndModel = DragAndDrop.Types.Model.createWithItems elementIds
      { model with DragAndDrop = dndModel; ContentMap = m }, Cmd.none
    | DndMsg msg ->
      let dndModel, cmd = DragAndDrop.Update.update msg model.DragAndDrop
      { model with DragAndDrop = dndModel }, Cmd.map DndMsg cmd
    | InputChange (elementId, newValue) ->
      match Map.tryFind elementId model.ContentMap with
      | Some oldContent ->
        let newValue = { oldContent with UserInput = newValue}
        let m = Map.add elementId newValue model.ContentMap
        { model with ContentMap = m }, Cmd.none
      | None ->
        let newValue = { UserInput = newValue; Name = "" }
        let m = Map.add elementId newValue model.ContentMap
        { model with ContentMap = m }, Cmd.none