namespace Pages

module CollectionDragAndDrop3 =
  open Feliz
  open Fable.React
  open Fable.React.Props
  open Browser.Dom
  open Browser.Types
  open Elmish
  open Elmish.React
  open Elmish.DragAndDrop3

  type Model = {
    DragAndDrop : DragAndDrop3.Model
    Content : (string * ReactElement) list
    ContentMap : Map<string, ReactElement>
  }

  let init() = 
    {
      DragAndDrop = DragAndDrop3.empty()
      Content = []
      ContentMap = Map.empty
    }

  type Msg =
  | Init
  | DndMsg of DragAndDrop3.Msg

  let mappedMsg msg = DndMsg msg

  // module DragStyles =
  //   // let slidingStyle =
  //   //   [
  //   //     Position PositionOptions.Absolute
  //   //     CSSProp.MarginLeft -20.
  //   //     CSSProp.MarginTop -20.
  //   //     CSSProp.TransitionDuration "0.6s"
  //   //     CSSProp.TransitionProperty "top"
  //   //   ] |> SlideStyle
  //   let dragStyling = [
  //     CSSProp.MarginLeft -130.
  //     CSSProp.MarginTop -50.
  //   ]
  //   let draggableStyle = [ yield! Styles.IsDraggable() ] |> DraggableStyle
  //   let previewStyle = [ yield! Styles.IsPreview() ] |> PreviewStyle
  //   let draggedStyle model = [ yield! Styles.IsDragged model; yield! dragStyling ] |> DraggedStyle

  //   let allDragStyles model = [  draggableStyle; previewStyle; draggedStyle model ]

  // module DragProps =
  //   let slidingProps cn id =
  //     SlideProps [
  //       ClassName cn
  //       Id id
  //     ]
  //   let draggableProps cn id = 
  //     DragProp.DraggableProps [
  //       ClassName cn
  //       Id id
  //     ]

  //   let draggedProps cn id = DragProp.DraggedProps [ ClassName cn; Id id ]
  //   let previewProps cn = DragProp.PreviewProps [ ClassName cn ]

  //   let allDragProps cn id = [ draggableProps cn id; draggedProps cn id; previewProps cn ]

  // let createDraggable model id (dispatch : Msg -> unit)  _class content =
  //   let props = [
  //     yield! DragStyles.allDragStyles model.DragAndDrop
  //     yield! DragProps.allDragProps _class id
  //   ]
  //   let slideProps = [DragProps.slidingProps _class id]
  //   Draggable.draggable model.DragAndDrop id (mappedMsg >> dispatch) slideProps props [content]

  let createDraggableTemplate className = {
    DragAndDrop3.DraggableTemplate.Empty() with
      DraggedElementStyles = Some [
          MarginLeft -130.
          MarginTop -50.
          Opacity 0.8
          Position PositionOptions.Fixed
          Cursor "grabbing"
      ]
      DraggedElementProperties = Some [
        ClassName (className + " dragged")
      ]
      DraggableElementStyles = Some [
        Cursor "grab"
      ]
      HoverPreviewElementStyles = Some [
        Opacity 0.2
        PointerEvents "None"
      ]
      DefaultClass = Some "content"
  }

  let createDraggable model id (dispatch : Msg -> unit)  _class content =
    let msging = Messaging.Create model.DragAndDrop id (mappedMsg >> dispatch)
    Draggable.draggable msging [
      DraggableProp.DraggedElement [
        Styling [
          // some manual fudging is required to make the dragged element appear under the cursor
          // in the right spot
          MarginLeft -130.
          MarginTop -50.
          Opacity 0.8
          Position PositionOptions.Fixed
          Cursor "grabbing"
        ]
        Properties [
          ClassName (_class + " dragged" )
        ]
      ]
      DraggableElement [
        Styling [ 
          Cursor "grab"
        ]
      ]
      HoverPreviewElement [
        Styling [
          Opacity 0.2
          PointerEvents "none"
        ]
      ]
      DefaultClass "content"
    ] [content]

  let createDropArea model _class (dispatch : Msg -> unit)  content =
    let props = AreaProps [ ClassName _class ]
    DropArea.dropArea model.DragAndDrop (mappedMsg >> dispatch) [ props ] content

  let view model (dispatch : Msg -> unit) =
    let template = createDraggableTemplate "content"
    // let content =
    //   model.DragAndDrop.ElementIds()
    //   |> List.map(fun li -> 
    //     li
    //     |> List.map (fun id ->
    //       let content = model.ContentMap.Item id
    //       createDraggable model id dispatch "content" content
    //     )
    //   )
    //   |> List.map (fun items ->
    //     createDropArea model "container" dispatch items
    //   )
    let dropAreaProps =
      [
        (ClassName "container") :> IHTMLProp
      ] |> AreaProps
    let dropAreaContent =
      model.DragAndDrop.ElementIds()
      |> List.map(fun li ->
        li
        |> List.map (fun id ->
          let content = model.ContentMap.Item id
          let msging = Messaging.Create model.DragAndDrop id (mappedMsg >> dispatch)
          (msging, [content])
        )
        |> DropArea.dropAreaWithTemplate model.DragAndDrop (mappedMsg >> dispatch) [dropAreaProps] template
      )
    div [ ClassName "wrapper" ] dropAreaContent

  let update msg model =
    match msg with
    | Init ->
      printfn "in init"
      let content =
        [
          div [] [ h3 [] [str "Content 0"]; p [] [ str "This is some content" ] ]
          div [] [ h3 [] [str "Content 1"]; p [] [ str "And this is more content" ] ]
          div [] [ h3 [] [str "Content 2"]; p [] [ str "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." ] ]
          div [] [ h3 [] [str "Content 3"]; p [] [ str "And this is yet more content" ] ]
          div [] [ h3 [] [str "Content 4"]; p [] [ str "some piece of content" ] ]

          div [] [ h3 [] [str "Content 5"]; p [] [ str "hello world" ] ]
          div [] [ h3 [] [str "Content 6"]; p [] [ str "this is another column" ] ]
          div [] [ h3 [] [str "Content 7"]; p [] [ str "content can be anything - including images"]; img [Src "marek-piwnicki-unsplash.jpg"; Style [ Width "128px"; Height "128px" ]] ]
        ] |> List.mapi (fun i c -> (sprintf "content-%i" i), c)
      let ids1, ids2 = content |> List.map fst |> List.splitAt 5
      let m = Map.ofList content
      let dndModel = DragAndDrop3.ModelFuncs.createWithItemsMultiList [ids1; ids2]
      { model with Content = content; DragAndDrop = dndModel; ContentMap = m }, Cmd.none
    | DndMsg msg ->
      let dndModel, cmd = DragAndDrop3.update msg model.DragAndDrop
      { model with DragAndDrop = dndModel }, Cmd.map DndMsg cmd