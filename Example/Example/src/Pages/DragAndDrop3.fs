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
    Content : (string * string) list
  }

  let init() = 
    {
      DragAndDrop = DragAndDrop3.empty()
      Content = []
    }

  type Msg =
  | Init
  | DndMsg of DragAndDrop3.Msg

  let mappedMsg msg = DndMsg msg

  module DragStyles =
    let slidingStyle =
      [
        Position PositionOptions.Absolute
        CSSProp.MarginLeft -20.
        CSSProp.MarginTop -20.
        CSSProp.TransitionDuration "0.6s"
        CSSProp.TransitionProperty "top"
      ] |> SlideStyle
    let draggableStyle = [ yield! Styles.IsDraggable() ] |> DraggableStyle
    let previewStyle = [ yield! Styles.IsPreview() ] |> PreviewStyle
    let draggedStyle model = [ yield! Styles.IsDragged model ] |> DraggedStyle

    let allDragStyles model = [ slidingStyle ; draggableStyle; previewStyle; draggedStyle model ]

  module DragProps =
    let slidingProps cn id =
      DragProp.SlideProps [
        ClassName cn
        Id id
      ]
    let draggableProps cn id = 
      DragProp.DraggableProps [
        ClassName cn
        Id id
      ]

    let draggedProps cn id = DragProp.DraggedProps [ ClassName cn; Id id ]
    let previewProps cn = DragProp.PreviewProps [ ClassName cn ]

    let allDragProps cn id = [ slidingProps cn id; draggableProps cn id; draggedProps cn id; previewProps cn ]

  module Listeners =
    let draggableListener model id dispatch =
      Listeners.defaultDraggable model id dispatch
      |> DraggableListener
    let draggedListener dispatch =
      Listeners.defaultDragListener dispatch
      |> DraggedListener
    let hoverListener model id dispatch =
      Listeners.defaultHoverListener model id dispatch 
      |> HoverListener

    let allListeners model id dispatch = 
      [ 
        draggableListener model id dispatch
        hoverListener model id dispatch
        draggedListener dispatch
      ]

  let createDraggable model id (dispatch : Msg -> unit)  _class content =
    let props = [
      yield! DragStyles.allDragStyles model.DragAndDrop
      yield! DragProps.allDragProps _class id
      yield! Listeners.allListeners model.DragAndDrop id (mappedMsg >> dispatch)
    ]
    Draggable.draggable model.DragAndDrop id props [content]

  let createDropArea model _class (dispatch : Msg -> unit)  content =
    let props = AreaProps [ ClassName _class ]
    let listeners =
      Listeners.defaultReleaseListener (mappedMsg >> dispatch)
      |> ReleaseListener
    DropArea.dropArea model.DragAndDrop [ props; listeners ] content

  let view model (dispatch : Msg -> unit) =
    let content =
      model.Content
      |> List.map (fun c ->
        createDraggable model (fst c) dispatch "content" (p [] [ str (snd c) ])
      )
    div [ ClassName "wrapper" ][
      createDropArea model "container" dispatch content
    ]

  let update msg model =
    match msg with
    | Init ->
      printfn "in init"
      let content =
        [
          "This is some content"
          "And this is more content"
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          "And this is yet more content"
          "final piece of content"
        ] |> List.mapi (fun i c -> (sprintf "content-%i" i), c)
      let ids = content |> List.map fst

      let mdl = DragAndDrop3.Model.createWithItems (ids)
      { model with Content = content; DragAndDrop = mdl }, Cmd.none
    | DndMsg msg ->
      let mdl, cmd = DragAndDrop3.update msg model.DragAndDrop
      { model with DragAndDrop = mdl }, Cmd.map DndMsg cmd