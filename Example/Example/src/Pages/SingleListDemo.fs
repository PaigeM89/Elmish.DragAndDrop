namespace Pages

module SingleListDemo =
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
      // SlidingElementStyles = Some [
      //   CSSProp.TransitionDuration 1.0
      //   PointerEvents "None"
      //   Position PositionOptions.Fixed
      // ]
      DefaultClass = Some "content"
  }

  let view model (dispatch : Msg -> unit) =
    let template = createDraggableTemplate "content"
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
        |> DropArea.dropArea model.DragAndDrop (mappedMsg >> dispatch) [dropAreaProps] template
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
      let ids = content |> List.map fst
      let m = Map.ofList content
      let dndModel = DragAndDrop3.ModelFuncs.createWithItems ids
      { model with Content = content; DragAndDrop = dndModel; ContentMap = m }, Cmd.none
    | DndMsg msg ->
      let dndModel, cmd = DragAndDrop3.update msg model.DragAndDrop
      { model with DragAndDrop = dndModel }, Cmd.map DndMsg cmd