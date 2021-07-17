namespace Pages

(*
  A somewhat common use for Drag And Drop, this example shows non-interactable elements
  in 2 categories, with the ability for the user to move items between those categories at will.
*)

module MultiListDemo =
  open Fable.React
  open Fable.React.Props
  open Elmish
  open Elmish.DragAndDrop

  type Model = {
    DragAndDrop : DragAndDropModel
    ContentMap : Map<string, ReactElement>
  }

  let initContent model =
    let allContent =
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
    let m = Map.ofList allContent
    let list1, list2 = allContent |> List.map fst |> List.splitAt 5 
    let dndModel = DragAndDropModel.createWithItemsMultiList [list1;list2]
    { model with ContentMap = m; DragAndDrop = dndModel }



  let init() = 
    {
      DragAndDrop = DragAndDropModel.Empty()
      ContentMap = Map.empty
    } |> initContent

  type Msg =
  | Init
  | DndMsg of DragAndDropMsg

  let mappedMsg msg = DndMsg msg

  let dragAndDropConfig = {
    DragAndDropConfig.Empty() with 
      DraggedElementStyles = Some [
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
      MoveThrottleTimeMs = Some (System.TimeSpan.FromMilliseconds 500.)
  }

  let createDraggable model elementId dispatch =
    let element =
      Map.tryFind elementId model.ContentMap
      |> Option.defaultValue (div [] [])
    ElementGenerator.Create elementId [ Cursor "grab" ] [ ClassName "content" ] [ element ]
    |> Draggable.asDragHandle model.DragAndDrop dragAndDropConfig dispatch

  let view model (dispatch : Msg -> unit) =
    let dispatch = mappedMsg >> dispatch
    let leftDropAreaProps : IHTMLProp list = [
      Style [
        MarginLeft "auto"
        Display DisplayOptions.Table
        Background "#33adff"
      ]
    ]
    let rightDropAreaProps : IHTMLProp list = [
      Style [
        MarginRight "auto"
        Display DisplayOptions.Table
        Background "#3300aa"
      ]
    ]
    let dropAreaContent =
      model.DragAndDrop.ElementIds()
      |> List.mapi(fun index li ->
        let props = if index % 2 = 0 then leftDropAreaProps else rightDropAreaProps
        li
        |> List.map (fun id ->
          createDraggable model id dispatch
        )
        |> DropArea.fromDraggables div props
      )
    let contextProps : IHTMLProp list = [
      Style [
        Background "#0066ff"
        Width "100%"
        Display DisplayOptions.Flex
      ]
    ]
    DragDropContext.context model.DragAndDrop dispatch div contextProps dropAreaContent

  let update msg model =
    match msg with
    | Init ->
      model, Cmd.none
    | DndMsg msg ->
      let dndModel, cmd = dragAndDropUpdate msg model.DragAndDrop
      { model with DragAndDrop = dndModel }, Cmd.map DndMsg cmd