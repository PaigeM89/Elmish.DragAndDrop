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
    DragAndDrop : DragAndDrop.Model.Model
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
      DragAndDrop = DragAndDrop.Model.Model.Empty()
      ContentMap = initContentMap()
    }

  type Msg =
  | Init
  | DndMsg of DragAndDrop.Model.Msg

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

  let createGenerator model elementId =
    let element =
      Map.tryFind elementId model.ContentMap
      |> Option.defaultValue (div [] [])
    ElementGenerator.Create elementId [ Cursor "grab" ] [ ClassName "content" ] [element]
    // A DragHandle can either be rendered "early" as part of an element (see the Handles example)
    // or can be Deferred to render later if it's the root element by which the DropArea will render content
    |> DragHandle.Deferred

  let view model (dispatch : Msg -> unit) =
    let dispatch = (mappedMsg >> dispatch)
    let dropAreaProps = [ (ClassName "container") :> IHTMLProp ]
    let dropAreaContent =
      // note that element Ids are always a list of lists, to accomodate multiple categories.
      // see the Multi List Demo for an example of that.
      model.DragAndDrop.ElementIds()
      |> List.map(fun li ->
        li
        |> List.map (fun id ->
          let content = createGenerator model id
          id, content
        )
        |> DropArea.fromDragHandles model.DragAndDrop dispatch dragAndDropConfig dropAreaProps
      )
    div [ ClassName "wrapper" ] dropAreaContent

  let update msg model =
    match msg with
    | Init ->
      let content = model.ContentMap |> Map.toList
      let ids = content |> List.map fst
      let dndModel = DragAndDrop.Model.createWithItems ids
      { model with DragAndDrop = dndModel }, Cmd.none
    | DndMsg msg ->
      let dndModel, cmd = DragAndDrop.Update.update msg model.DragAndDrop
      { model with DragAndDrop = dndModel }, Cmd.map DndMsg cmd