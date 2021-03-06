namespace Pages


(*
  The most straighfoward use of drag and drop, this shows non-interactable elements in a single list
  that the user can move around.
*)

module SingleListDemo =
  open Fable.React
  open Fable.React.Props
  open Elmish
  open Elmish.DragAndDrop

  type Model = {
    /// Stores information for the Drag & Drop internal state
    DragAndDrop : DragAndDropModel
    /// Lookup when rendering content; order is fed from the Drag & Drop model
    ContentMap : Map<string, ReactElement>
  }


  let initContentMap() =
    [
      div [] [ h3 [] [str "Content 0"]; p [] [ str "This is some content" ] ]
      div [] [ h3 [] [str "Content 1"]; p [] [ str "And this is more content" ] ]
      div [] [ h3 [] [str "Content 2"]; p [] [ str "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." ] ]
      div [] [ h3 [] [str "Content 3"]; p [] [ str "And this is yet more content" ] ]
      div [] [ h3 [] [str "Content 4"]; p [] [ str "some piece of content" ] ]
      div [] [ h3 [] [str "Content 5"]; p [] [ str "hello world" ] ]
      div [] [ h3 [] [str "Content 6"]; p [] [ str "this is another column" ] ]
      div [] [ h3 [] [str "Content 7"]; p [] [ str "content can be anything - including images"]; img [Src "marek-piwnicki-unsplash.jpg"; Style [ Width "128px"; Height "128px" ]] ]
    ]
    |> List.mapi (fun i c -> (sprintf "content-%i" i), c)
    |> Map.ofList

  let init() =
    {
      DragAndDrop = DragAndDropModel.Empty()
      ContentMap = initContentMap()
    }

  type Msg =
  | Init
  | DndMsg of DragAndDropMsg

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

  let createDragHandle model elementId dispatch =
    let element =
      Map.tryFind elementId model.ContentMap
      |> Option.defaultValue (div [] [])
    let handleId = elementId + "-handle"
    ElementGenerator.Create handleId [ Cursor "grab" ] [] [element]
    |> DragHandle.dragHandle model.DragAndDrop elementId dispatch

  let createDraggable model elementId dispatch =
    let handle = createDragHandle model elementId dispatch
    ElementGenerator.Create elementId [] [ ClassName "content" ] [ handle ]
    |> Draggable.draggable model.DragAndDrop dragAndDropConfig dispatch

  let view model (dispatch : Msg -> unit) =
    let dispatch = (mappedMsg >> dispatch)
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
          createDraggable model id dispatch
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
    let content = dropAreaContent
    DragDropContext.context model.DragAndDrop dispatch div props content

  let update msg model =
    match msg with
    | Init ->
      let content = model.ContentMap |> Map.toList
      let ids = content |> List.map fst
      let dndModel = DragAndDropModel.createWithItems ids
      { model with DragAndDrop = dndModel }, Cmd.none
    | DndMsg msg ->
      let dndModel, cmd = dragAndDropUpdate msg model.DragAndDrop
      { model with DragAndDrop = dndModel }, Cmd.map DndMsg cmd