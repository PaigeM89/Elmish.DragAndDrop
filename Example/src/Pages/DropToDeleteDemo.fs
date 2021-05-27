namespace Pages

module DropToDeleteDemo =
  open Fable.React
  open Fable.React.Props
  open Elmish
  open Elmish.DragAndDrop

  type DeleteAction = 
  | Hover of elementId : string
  | Deleted of elementId : string

  type Model = {
    /// Stores information for the Drag & Drop internal state
    DragAndDrop : DragAndDropModel
    /// Lookup when rendering content; order is fed from the Drag & Drop model
    ContentMap : Map<string, ReactElement>
    DeleteAction : DeleteAction option
  }

  let initialValues = [ 
      "content-0", div [] [ h3 [] [str "Content 0"]; p [] [ str "This is some content" ] ]
      "content-1", div [] [ h3 [] [str "Content 1"]; p [] [ str "And this is more content" ] ]
      "content-2", div [] [ h3 [] [str "Content 3"]; p [] [ str "And this is yet more content" ] ]
  ]

  let init() =
    {
      DragAndDrop = DragAndDropModel.Empty()
      ContentMap = initialValues |> Map.ofList
      DeleteAction = None
    }

  type Msg =
  | Init
  | DndMsg of DragAndDropMsg
  | HoverOverDelete of elementId : string
  | Deleted of elementId : string

  let mappedMsg msg = DndMsg msg

  let dragAndDropConfig = {
    DragAndDropConfig.Empty() with
      DraggedElementStyles = Some [
          // manually shift the item to fit under the cursor in an intuitive way
          MarginLeft -130.
          MarginTop -50.
          Opacity 0.8
          Position PositionOptions.Fixed
          Cursor "grabbing"
          Background "darkcyan"
      ]
      HoverPreviewElementStyles = Some [
        Opacity 0.2
        PointerEvents "None"
      ]
  }

  let createInitButton dispatch =
    let prop = OnClick (fun ev -> ev.preventDefault(); Init |> dispatch)
    let style = Style [
      Display DisplayOptions.Flex
    ]
    button [ prop; style ] [ p [] [ str "Initialize / Reset" ] ]

  let createDragHandle model elementId rootElementId dispatch =
    let element =
      Map.tryFind elementId model.ContentMap
      |> Option.defaultValue (div [] [])
    let handleId = elementId + "-handle"
    ElementGenerator.Create handleId [ Cursor "grab" ] [] [element]
    |> DragHandle.dragHandle model.DragAndDrop elementId dispatch

  let createDraggable model elementId dispatch =
    let rootId = elementId // making these equal makes the whole thing draggable
    let handle = createDragHandle model elementId rootId dispatch
    ElementGenerator.Create rootId [] [ ClassName "content" ] [ handle ]
    |> Draggable.draggable model.DragAndDrop dragAndDropConfig dispatch

  let createDropBucket model dispatch =
    let onHover = (fun _ id -> printfn "on hover for delete bucket %A triggered" id; HoverOverDelete id |> dispatch)
    let onDrop = (fun _ id -> printfn "on drop for delete block %A triggered" id; Deleted id |> dispatch )
    let styles = [
      //if not isHovered then Background "#434343" else Background "#0044AA"
      match model.DeleteAction with
      | Some (Hover _) -> Background "#337799"
      | Some (DeleteAction.Deleted _) -> Background "#997733"
      | _ -> Background "#345678"
      Display DisplayOptions.Flex
    ]
    ElementGenerator.Create "delete-bucket" styles [] [ h2 [] [ str "Drag here to delete!" ] ]
    |> DropArea.asBucket model.DragAndDrop dragAndDropConfig onHover onDrop (mappedMsg >> dispatch)

  let view model (dispatch : Msg -> unit) =
    let dropAreaProps = [ 
      Style [
        Background "#33adff"
        Display DisplayOptions.Table
        Width 400
        MarginLeft "auto"
        MarginRight "auto"
      ] :> IHTMLProp
    ]
    let dropAreaContent =
      // note that element Ids are always a list of lists, to accomodate multiple categories.
      // see the Multi List Demo for an example of that.
      model.DragAndDrop.ElementIds()
      |> List.map(fun li ->
        li
        |> List.map (fun id ->
          createDraggable model id (mappedMsg >> dispatch)
        )
        |> DropArea.fromDraggables div dropAreaProps
      )
    let props : IHTMLProp list = [ 
      ClassName "wrapper"
      Style [
        Display DisplayOptions.Flex
        MarginLeft "auto"
        MarginRight "auto"
        Width "100%"
      ]
    ]
    let deleteDropArea = createDropBucket model dispatch
    let content = [
      yield createInitButton (dispatch)
      yield! dropAreaContent
      yield deleteDropArea
    ]
    DragDropContext.context model.DragAndDrop (mappedMsg >> dispatch) div props content

  let update msg model =
    match msg with
    | Init ->
      let model = init()
      let content = model.ContentMap |> Map.toList
      let ids = content |> List.map fst
      let dndModel = DragAndDropModel.createWithItems ids
      { model with DragAndDrop = dndModel; DeleteAction = None }, Cmd.none
    | DndMsg msg ->
      let dndModel, cmd = dragAndDropUpdate msg model.DragAndDrop
      { model with DragAndDrop = dndModel; DeleteAction = None  }, Cmd.map DndMsg cmd
    | HoverOverDelete hoverElementId ->
      let a = Hover hoverElementId
      { model with DeleteAction = Some a; }, Cmd.none
    | Deleted deletedId ->
      let a = DeleteAction.Deleted deletedId
      let items = model.ContentMap |> Map.remove deletedId
      let dnd = model.DragAndDrop |> DragAndDropModel.removeItem deletedId
      { model with DeleteAction = Some a; ContentMap = items; DragAndDrop = dnd}, Cmd.none