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

  /// do i need this?
  //type Rect = { x : float; y : float; wedight : float; height: float }

  type ListIndex = int
  type Index = int
  type ItemLocation = ListIndex * Index * ElementId
  let locListIndex = fun (x, _, _) -> x
  let locId = fun (_, _, id) -> id
  
  let previewId id = id + "-preview"

  type Model = {
    Cursor : Coords
    Offset: Coords option
    Moving : ElementId option
    Items: (ItemLocation * ReactElement) list list
    // OnDrag : (ElementId -> unit) option
    // OnDragEnter: (ElementId -> unit) option
  }


  let initItemLocations lists =
    lists
    |> List.mapi(fun i li ->
      li
      |> List.mapi (fun j x ->
        (i, j, fst x), snd x
      )
    )

  let init() = {
    Cursor = { x = 0. ; y = 0.}
    Offset = None
    Moving = None
    Items = [[
      ("draggable-0", p [] [ str "This is some content" ])
      ("draggable-1", div [] [
            p [] [ str "And some content with an input box" ]
            input [ ]
      ])
      ("draggable-2", p [ ] [ str "And yet even more" ])
    ];
    [
      ("draggable-3", p [ ] [ str "This is is also content" ])
      ("draggable-4", p [ ] [ str "And some more content" ])
      ("draggable-5", h3 [ ] [ str "And more content, but different" ])
    ]] |> initItemLocations
    // OnDrag = None
    // OnDragEnter = None
  }

  type Msg =
  | DragStart of loc : ItemLocation * start : Coords * offset : Coords
  | OnDrag of elementId : ElementId * coords : Coords
  | DragEnter of target : ElementId
  | DragLeave
  | DragOver of loc : ItemLocation
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

  let IsDraggable loc (t : string) dispatch : IHTMLProp list= [
    Style [
      CSSProp.Custom ("draggable", true)
      DraggableType t
      CSSProp.Cursor "grab"
    ]
    DOMAttr.OnMouseDown(fun ev ->
      ev.preventDefault()
      let o = getOffset ev (locId loc)
      (loc, fromME ev, o) |> DragStart |> dispatch
    )
  ]

  let IsListener loc dispatch : IHTMLProp list = [
    Style [
      CSSProp.Cursor "none"
    ]
    DOMAttr.OnMouseEnter(fun ev ->
      ev.preventDefault()
      loc |> DragOver |> dispatch
    )
  ]

  let IsDragged id t {Coords.x = x; y = y} dispatch : IHTMLProp list = [
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

  let draggable model dispatch loc _class content =
    let id = locId loc
    match model.Moving with
    | Some elementId when elementId = id ->
      let coords = model.Cursor -- model.Offset
      div [] [
        div [ yield! IsDragged id "test" coords dispatch; ClassName (_class + " dragged"); Id id ] [ content ]
        div [ yield! IsPreview(); ClassName (_class); Id (previewId id) ] [content]
      ]
    | Some _ ->
      // if we have an active drag, dont render anything as draggable
      div [ ClassName _class; Id id ] [ content ]
    | None ->
      div [ yield! IsDraggable loc "test" dispatch; ClassName _class; Id id ] [ content ]

  let sortedDraggables model dispatch _class (items : (ItemLocation * ReactElement) list) =
    match model.Moving with
    | Some dei ->
      /// an element is being dragged
      items |> List.map (fun (loc, content) ->
        let id = locId loc
        if id = dei then
          let coords= model.Cursor -- model.Offset
          let hover = div [ yield! IsDragged id "test" coords dispatch; ClassName (_class + " dragged"); Id id ] [content]
          let preview = div [ yield! IsPreview(); ClassName _class ] [ content ]
          div [] [ hover; preview ]
        else
          div [ yield! IsListener loc dispatch; ClassName _class; Id id ] [ content ]
      )
    | None ->
      /// nothing is being dragged
      items |> List.map (fun (loc, content) ->
        div [ yield! IsDraggable loc "test" dispatch; ClassName _class; Id (locId loc)] [ content ]
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

    let SortedDraggable loc draggedElementId (styles : Styles) (dispatch : Msg -> unit) content =
      let id = locId loc
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
        let listeners = IsListener loc dispatch
        div [ Style styles.Regular; yield! listeners ] content
      | None ->
        // nothing is being dragged
        let f = (fun ev -> (loc, (fromME ev), getOffset ev id) |> DragStart |> dispatch)
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
        sortedDraggables model dispatch "content" model.Items.[0]
      )

    let right =
      let items = 
        model.Items.[1]
        |> sortedDraggables model dispatch "content"
      Components.DropArea("right-container", "test", "container right", model.Moving.IsSome, dispatch, items)

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

  let private split i li =
    let len x = List.length x
    let first = if len li > i then List.take i li else li
    let second = if len li <= i then [] else List.skip i li
    first, second

  let private getItemContent id model =
    let rec traverseList li =
      match li with
      | [] -> None
      | (loc, content) :: xs ->
        if locId loc = id then Some content else traverseList xs
    let rec traverseLists lists =
      match lists with
      | [] -> None
      | x :: xs -> 
        match traverseList x with
        | Some v -> Some v
        | None -> traverseLists xs
    traverseLists model.Items


  let private removeElement id model =
    printfn "Removing %s from lists" id
    // let mutable removedElement = None
    //purge all previews
    let previewId = previewId id
    let isTarget (eleId, content) =
      if (locId eleId) = id then
        // removedElement <- Some (eleId, content)
        true
      elif (locId eleId) = previewId then
        true
      else
        false

    let filterList li =
      li
      |> List.filter (fun x -> 
        let r = not(isTarget x)
        printfn "Checking item %A for deleting id %s, got %A" (fst x) id r
        r
      )

    let updatedLists =
      model.Items
      |> List.map(fun li -> filterList li)
    updatedLists //, removedElement

  let private insertAt (listIndex, index, id) item (lists: (ItemLocation * ReactElement) list list) =
    let li = lists.[listIndex]
    let h, t = split index li
    let v = (listIndex, index, id), item
    h @ (v :: t)

  let private replaceListAtIndex listIndex li items =
    items
    |> List.mapi (fun i x -> if i = listIndex then li else x)

  let private moveItem (listIndex, index, _) (newElementId) model =
    // the old element should be automatically shifted as part of list shuffling
    let removedElementOpt = getItemContent newElementId model
    let allItems = removeElement newElementId model
    printfn "All Items are %A" allItems
    match removedElementOpt with
    | Some ele ->
      let newList = insertAt (listIndex, index, newElementId) (ele) allItems
      let newItems = replaceListAtIndex listIndex newList allItems
      { model with Items = newItems }
    | None ->
      { model with Items = allItems }

  let update msg model  = //(li : ReactElement list) =
    match msg with
    | DragStart (loc, startCoords, offset) ->
      { model with Moving = Some (locId loc); Cursor = startCoords; Offset = Some offset }, Cmd.none
    | OnDrag (element, coords) ->
      // match model.OnDrag with
      // | Some f -> f element
      // | None -> ()
      {model with Cursor = coords }, Cmd.none
    | DragEnter element ->
      // match model.OnDragEnter with
      // | Some f -> f element
      // | None -> ()
      model, Cmd.none
    | DragOver loc ->
      match model.Moving with
      | Some id ->
        let model = moveItem loc id model
        model, Cmd.none
      | None ->
        model, Cmd.none
    | DragEnd ->
      { model with Moving = None }, Cmd.none
    | _ -> model, Cmd.none
