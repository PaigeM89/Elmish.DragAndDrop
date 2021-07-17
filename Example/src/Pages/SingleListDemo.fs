namespace Pages


(*
  The most straighfoward use of drag and drop, this shows non-interactable elements in a single list
  that the user can move around.
*)

module SingleListDemo =
  open Fable.React
  open Fable.React.Props
  open Elmish
  open Elmish.DragAndDrop2

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

  let view model (dispatch : Msg -> unit) =
    let dispatch = (mappedMsg >> dispatch)

    let dropAreaContent =
      // get all the element Ids in a single list.
      // we don't have multiple categories to concern ourselves with.
      model.DragAndDrop.ElementIdsSingleList()
      // we collect here because sometimes the Draggable returns multiple items
      |> List.collect(fun id ->
        // look up the content in our model's content map
        let content = Map.tryFind id model.ContentMap |> Option.defaultValue (div [] [ str "Unable to find content" ])
        
        // create the draggable
        Draggable.SelfHandle
          model.DragAndDrop
          dragAndDropConfig
          dispatch
          id
          div
          [ Cursor "grab" ]
          [ ClassName "content"; Id id ]
          [ content ]
      )

    let dropAreaProps = [ 
      Style [
        Background "#33adff"
        Display DisplayOptions.Table
        Width 400
        MarginLeft "auto"
        MarginRight "auto"
      ] :> IHTMLProp
    ]

    let dropArea =
      DropArea.DropArea 
        model.DragAndDrop
        dragAndDropConfig
        (MouseEventHandlers.Empty())
        dispatch
        "drop-area"
        div
        dropAreaProps
        dropAreaContent
    
    let props : IHTMLProp list = [ 
      ClassName "wrapper"
      Style [
        MarginLeft "auto"
        MarginRight "auto"
        Width "100%"
      ]
    ]
    DragDropContext.Context model.DragAndDrop dispatch div props [dropArea]

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