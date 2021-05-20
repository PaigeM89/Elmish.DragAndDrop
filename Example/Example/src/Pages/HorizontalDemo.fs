namespace Pages

module HorizontalDemo =
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
    Content : (string * ReactElement list) list
    ContentMap : Map<string, ReactElement list>
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
      DefaultClass = None
  }

  let view model (dispatch : Msg -> unit) =
    let template = createDraggableTemplate "content"
    let dropAreaProps =
      [
        //(ClassName "horizontal-container") :> IHTMLProp
        Style [
          CSSProp.PaddingTop 10
        ] :> IHTMLProp
      ] |> AreaProps
    let dragHandleProps : IHTMLProp list = [
      Style [
        Margin 10
        Background "#33ccff"
        CSSProp.Padding 5
        Border "1px solid black"
        Width 150
        CSSProp.MinHeight 50
        Display DisplayOptions.InlineBlock
      ]
    ]
    let dropAreaContent =
      model.DragAndDrop.ElementIds()
      |> List.map(fun li ->
        li
        |> List.map (fun id ->
          let msging = Messaging.Create model.DragAndDrop id (mappedMsg >> dispatch)
          let content = model.ContentMap.Item id |> DragHandle.dragHandle msging dragHandleProps 
          (msging, [content])
        )
        |> DropArea.dropArea model.DragAndDrop (mappedMsg >> dispatch) [dropAreaProps] template
      )
    div 
      [ 
        Style [
          Margin "auto"
          Background "#0066ff"
        ]
      ]
      dropAreaContent

  let update msg model =
    match msg with
    | Init ->
      printfn "in init"
      let content =
        [
          [ h3 [] [str "Content 0"]; p [] [ str "This is some content" ] ]
          [ h3 [] [str "Content 1"]; p [] [ str "And this is more content" ] ]
          [ h3 [] [str "Content 3"]; p [] [ str "And this is yet more content" ] ]
          [ h3 [] [str "Content 4"]; p [] [ str "some piece of content" ] ]
          [ h3 [] [str "Content 5"]; p [] [ str "hello world" ] ]
          [ h3 [] [str "Content 6"]; p [] [ str "this is another column" ] ]
          [ h3 [] [str "Content 7"]; p [] [ str "content can be anything - including images"]; img [Src "marek-piwnicki-unsplash.jpg"; Style [ Width "128px"; Height "128px" ]] ]
        ] |> List.mapi (fun i c -> (sprintf "content-%i" i), c)
      let ids = content |> List.map fst
      let m = Map.ofList content
      let dndModel = DragAndDrop3.ModelFuncs.createWithItems ids
      { model with Content = content; DragAndDrop = dndModel; ContentMap = m }, Cmd.none
    | DndMsg msg ->
      let dndModel, cmd = DragAndDrop3.update msg model.DragAndDrop
      { model with DragAndDrop = dndModel }, Cmd.map DndMsg cmd