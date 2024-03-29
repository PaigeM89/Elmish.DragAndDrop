namespace Pages

(*
  This demo shows how you can create "handles", 
  elements that the user will click on, to drag a parent element around.
  This is useful if the parent element contains elements that the user will want to interact with,
  such as input elements.
*)

module HandlesDemo =
  open Fable.React
  open Fable.React.Props
  open Elmish
  open Elmish.DragAndDrop

  type ContentKey = string
  type ContentType =
  | UserInput of userInput : string * name : string
  | Output of content : string * name : string
  
  let getName ct =
    match ct with
    | UserInput (_, n) -> n
    | Output (_, n) -> n

  type Model = {
    DragAndDrop : DragAndDropModel
    ContentMap : Map<ContentKey, ContentType>
  }

  let init() = 
    {
      DragAndDrop = DragAndDropModel.Empty()
      ContentMap = Map.empty
    }

  type Msg =
  | Init
  | DndMsg of DragAndDropMsg
  | InputChange of elementId : string * newValue : string

  let mappedMsg msg = DndMsg msg

  let dragAndDropConfig = {
    DragAndDropConfig.Empty() with
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
  let dragAndDropCategoryKey = "default-category"

  let inputValueLookup model elementId =
    Map.tryFind elementId model.ContentMap
    |> Option.defaultValue (("Unknown element content", "Unknown element") |> Output)


  let generateHandlesAndContent dndModel handleStyles rootElementId dispatch (ct : ContentType) =
    let handleId = rootElementId + "-handle"
    match ct with
    | UserInput (value, name) ->
      [
        DragHandle.Handle
                dndModel
                dragAndDropCategoryKey
                rootElementId
                (mappedMsg >> dispatch)
                div
                [ Id handleId; Style handleStyles ] 
                [
                  h3 [] [ str name ]
                ]
        input [
          Value value
          OnChange (fun ev ->
            let v = ev.Value
            // we track the current state of user input by the root draggable's ID, not the input id (which we dont set)
            InputChange (rootElementId, v) |> (dispatch))
        ]
      ]
    | Output (value, name) ->
      [
        DragHandle.Handle
          dndModel
          dragAndDropCategoryKey
          rootElementId
          (mappedMsg >> dispatch)
          div
          [ Id handleId; Style handleStyles ] 
          [
            h3 [] [ str name ]
          ]
        p [] [ str value ]
      ]

  let createContent dndModel (rootElementId : string) dispatch (value : ContentType) =
    let handleStyles = if dndModel.Moving.IsSome then [] else [ Cursor "grab" ]
    generateHandlesAndContent dndModel handleStyles rootElementId dispatch value
    |> Draggable.InnerHandle
            dndModel
            dragAndDropCategoryKey
            dragAndDropConfig
            (mappedMsg >> dispatch)
            rootElementId
            div 
            defaultStyles
            [ Id rootElementId ]

  let view model (dispatch : Msg -> unit) =
    let dropAreaProps : IHTMLProp list = [
      Style [
        MarginLeft "auto"
        MarginRight "auto"
        Display DisplayOptions.Table
        Background "#33adff"
      ]
    ]
    let dropAreaContent =
      model.DragAndDrop.ElementIdsForCategorySingleList dragAndDropCategoryKey
      |> List.collect (fun (rootElementId) ->
        inputValueLookup model rootElementId
        |> createContent model.DragAndDrop rootElementId dispatch
      )
      |> DropArea.DropArea 
          model.DragAndDrop
          dragAndDropCategoryKey
          dragAndDropConfig
          (MouseEventHandlers.Empty())
          (mappedMsg >> dispatch)
          "drop-area"
          div
          dropAreaProps
    DragDropContext.Context model.DragAndDrop (mappedMsg >> dispatch)
      div [
        Style [
          Background "#0066ff"
          Width "100%"
        ]
      ][
        p [] [ str "Drag an element by the title. Interact with the elements inside any list item as normal." ]
        dropAreaContent
      ]

  let update msg model =
    match msg with
    | Init ->
      let content = [
        for i in 1..8 do 
          if i % 2 = 0 then
            yield (sprintf "input-%i" i), UserInput ("", (sprintf "Input %i" i))
          else
            yield (sprintf "output-%i" i), Output (sprintf "Generated output #%i" i, sprintf "Output %i" i)
      ]
      let elementIds = content |> List.map (fst)
      let m = content |> Map.ofList
      let dndModel = DragAndDropModel.createWithItems elementIds
      { model with DragAndDrop = dndModel; ContentMap = m }, Cmd.none
    | DndMsg msg ->
      let dndModel, cmd = dragAndDropUpdate msg model.DragAndDrop
      { model with DragAndDrop = dndModel }, Cmd.map DndMsg cmd
    | InputChange (elementId, newValue) ->
      match Map.tryFind elementId model.ContentMap with
      | Some (UserInput (_, name)) ->
        let newValue = UserInput (newValue, name)
        let m = Map.add elementId newValue model.ContentMap
        { model with ContentMap = m }, Cmd.none
      | _ ->
        let newValue = Output (sprintf "Element %s not found" elementId, "Not found")
        let m = Map.add elementId newValue model.ContentMap
        { model with ContentMap = m }, Cmd.none