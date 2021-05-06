namespace Pages

module CollectionDragAndDrop2 =
  open Feliz
  open Fable.React
  open Fable.React.Props
  open Browser.Dom
  open Browser.Types
  open Elmish
  open Elmish.React
  
  /// todo: 
  ///   sorting
  ///   animations
  ///   category changing

  type ElementId = string

  type Coords = { x : float; y : float}
      with
      static member (+) (a, b) = { x = a.x + b.x; y = a.y + b.y }
      static member (++) (a, b: Coords option) =
        match b with
        | Some b -> a + b
        | None -> a
      static member (-) (a, b) = { x = a.x - b.x; y = a.y - b.y }
      static member (--) (a, b : Coords option) =
        match b with
        | Some b -> a - b
        | None -> a
  let coords x y = { x = x; y = y }
  let fromME (ev : MouseEvent) = { x = ev.clientX; y = ev.clientY }

  type Rect = { x : float; y : float; wedight : float; height: float }

  type Model = {
    Cursor : Coords
    Offset: Coords option
    Moving : ElementId option
    OnDrag : (ElementId -> unit) option
    OnDragEnter: (ElementId -> unit) option
  }

  let init() = {
    Cursor = { x = 0. ; y = 0.}
    Offset = None
    Moving = None
    OnDrag = None
    OnDragEnter = None
  }

  type Msg =
  | DragStart of elementId : ElementId * index : int * start : Coords * offset : Coords
  | OnDrag of elementId : ElementId * coords : Coords
  | DragEnter of target : ElementId
  | DragLeave
  | DragOver of elementId : ElementId * index : int
  | DragEnd

  let getDraggedElement id =
    let doc = Browser.Dom.document
    let ele = doc.getElementById(id)
    ele

  let getOffset ev id =
    let ele = getDraggedElement id
    let rect = ele.getBoundingClientRect()
    let coords = fromME ev
    let x = coords.x - rect.left
    let y = coords.y - rect.top
    { x = x; y = y}


  let DraggableType (t : string) = CSSProp.Custom ("draggableType", t)
  let IsDropArea (t : string) = CSSProp.Custom ("drop-bucket", t)

  let IsDraggable id index (t : string) dispatch : IHTMLProp list= [
    Style [
      CSSProp.Custom ("draggable", true)
      DraggableType t
      CSSProp.Cursor "grab"
    ]
    DOMAttr.OnMouseDown(fun ev ->
      ev.preventDefault()
      let o = getOffset ev id
      (id, index, fromME ev, o) |> DragStart |> dispatch
    )
  ]

  let IsListener id index dispatch : IHTMLProp list = [
    Style [
      CSSProp.Cursor "none"
    ]
    DOMAttr.OnMouseOver(fun ev ->
      ev.preventDefault()
      (id, index) |> DragOver |> dispatch
    )
  ]

  let IsDragged id index t {Coords.x = x; y = y} dispatch : IHTMLProp list = [
    Style [
      CSSProp.Custom ("draggable", true)
      DraggableType t
      CSSProp.Cursor "grabbing"
      CSSProp.Position PositionOptions.Fixed
      CSSProp.Left x
      CSSProp.Top y
      Opacity 0.8f
    ]
    OnMouseMove(fun ev ->
      ev.preventDefault()
      let offset = getOffset ev id
      OnDrag (id, offset) |> dispatch
    )
    OnMouseUp (fun ev -> ev.preventDefault();  DragEnd |> dispatch)
  ]

  let IsPreview() : IHTMLProp list = [
    Style [
      Opacity 0.2f
    ]
  ]

  let draggable model dispatch id index _class content =
    match model.Moving with
    | Some elementId when elementId = id ->
      let coords = model.Cursor -- model.Offset
      div [] [
        div [ yield! IsDragged id index "test" coords dispatch; ClassName (_class + " dragged"); Id id ] [ content ]
        div [ yield! IsPreview(); ClassName (_class) ] [content]
      ]
    | Some _ ->
      // if we have an active drag, dont render anything as draggable
      div [ ClassName _class; Id id ] [ content ]
    | None ->
      div [ yield! IsDraggable id index "test" dispatch; ClassName _class; Id id ] [ content ]

  let sortedDraggables model dispatch _class (items : (string * ReactElement) list) =
    match model.Moving with
    | Some dei ->
      /// an element is being dragged
      items |> List.mapi (fun i (id, content) -> 
        if id = dei then
          let coords= model.Cursor -- model.Offset
          let hover = div [ yield! IsDragged id i "test" coords dispatch; ClassName (_class + " dragged"); Id id ] [content]
          let preview = div [ yield! IsPreview(); ClassName _class ] [ content ]
          div [] [ hover; preview ]
        else
          div [ yield! IsListener id i dispatch; ClassName _class; Id id ] [ content ]
      )
    | None ->
      /// nothing is being dragged
      items |> List.mapi (fun i (id, content) ->
        div [ yield! IsDraggable id i "test" dispatch; ClassName _class; Id id] [ content ]
      )
    

  module Components =
    open Feliz

    type Styles = {
      Regular : CSSProp list
      Hover : CSSProp list
      Preview : CSSProp list
      Area : CSSProp list
    } with
      static member Empty() = {
        Regular = []
        Hover = []
        Preview = []
        Area = []
      }
      static member Default(regular, hover, preview, area) = {
        Regular = regular
        Hover = hover @ [ Opacity 0.8; Position PositionOptions.Fixed; ZIndex 9999; Margin 0 ]
        Preview = preview @ [ Opacity 0.2 ]
        Area = area
      }

    let SortedDraggable id index draggedElementId (styles : Styles) (dispatch : Msg -> unit) content =
      match draggedElementId with
      | Some dei when dei = id ->
        // draw the regular element, with the preview styles
        let preview = div [ Style styles.Preview ] content
        let hover = div [ Style styles.Hover ] content
        div [] [
          preview
          hover
        ]
      | Some _ ->
        //something is being dragged, but it's not this.
        let listener = OnMouseOver
        div [ Style styles.Regular;  ] content
      | None ->
        // nothing is being dragged
        let f = (fun ev -> (id, index, (fromME ev), getOffset ev id) |> DragStart |> dispatch)
        div [ Style styles.Regular; OnMouseDown f ] content

    /// i dont really like how any of this turned out

    let SortedDropArea id draggedElementId (styles : Styles) (dispatch : Msg -> unit) content =
      match draggedElementId with
      | Some dei ->
        div [
          Id id
          OnMouseUp (fun _ -> DragEnd |> dispatch)
          OnMouseMove (fun ev -> OnDrag (dei, fromME ev) |> dispatch )
          // todo: supply this some other way
          ClassName "drop-area"
        ] content
      | None ->
        div [
          Id id
          Style styles.Area
        ] content

    [<ReactComponent>]
    let DropArea (id, _type, _class, isDragging, dispatch, contents) = 
      div [ 
        Id id
        ClassName ("drop-area " + _class)
        Style [
          Custom ("dropareaType", _type)
          Height "200px"
        ]
        if isDragging then
          OnMouseMove(fun ev ->
            ev.preventDefault()
            let c = coords ev.clientX ev.clientY
            OnDrag (id, c) |> dispatch
          )
          OnMouseUp (fun ev -> ev.preventDefault();  DragEnd |> dispatch)
      ] contents

  let dropArea model dispatch = 
    let left = 
      Components.DropArea("left-container", "test", "container left", model.Moving.IsSome, dispatch, 
        [
            draggable model dispatch "draggable-0" 0 "content" (p [] [ str "This is some content" ])
            div [] [
                p [] [ str "And some content with an input box" ]
                input [ ]
            ] |> draggable model dispatch "draggable-1" 1 "content"
            p [ ] [ str "And yet even more" ]  |> draggable model dispatch "draggable-2" 2 "content"
        ]
      )

    let right =
      let items = 
        [
          ("draggable-3", p [ ] [ str "This is is also content" ])
          ("draggable-4", p [ ] [ str "And some more content" ])
          ("draggable-5", h3 [ ] [ str "And more content, but different" ])
        ]
        |> sortedDraggables model dispatch "content"
      Components.DropArea("right-container", "test", "container right", model.Moving.IsSome, dispatch, items)
        // div [Id "right-container"; ClassName "container right" ] [
        //     p [ ClassName "content dad-unselectable" ] [ str "This is is also content" ]
        //     p [ ClassName "content dad-unselectable" ] [ str "And some more content" ]
        //     h3 [ ClassName "content dad-unselectable" ] [ str "And more content, but different" ]
        // ]

    div [ ] [
        left
        right
    ]

  let view model dispatch =
    div [
      //Style [ CSSProp.AlignContent AlignContentOptions.Center; Display DisplayOptions.Flex ]
    ] [
        h2 [] [ str "Drag and drop 2 dropping boogaloo"]
        div [ ClassName "wrapper" ] [
            dropArea model dispatch
        ]
    ]

  let update msg model  = //(li : ReactElement list) =
    match msg with
    | DragStart (element, index, startCoords, offset) ->
      { model with Moving = Some element; Cursor = startCoords; Offset = Some offset }, Cmd.none
    | OnDrag (element, coords) ->
      match model.OnDrag with
      | Some f -> f element
      | None -> ()
      {model with Cursor = coords }, Cmd.none
    | DragEnter element ->
      match model.OnDragEnter with
      | Some f -> f element
      | None -> ()
      model, Cmd.none
    | DragOver (id, index) -> // todo : this
      printfn "drag over id %s at index %i" id index
      model, Cmd.none
    | DragEnd ->
      { model with Moving = None }, Cmd.none
    | _ -> model, Cmd.none
