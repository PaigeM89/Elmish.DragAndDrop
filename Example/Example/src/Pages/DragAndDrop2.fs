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
  ///   category blocking
  ///   removal when dropping off target
  ///   make input fields usable lol

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
      member this.Scale x = { x = this.x * x; y = this.y * x}
  let coords x y = { x = x; y = y }
  let fromME (ev : MouseEvent) = { x = ev.clientX; y = ev.clientY }

  type ListIndex = int
  type Index = int
  type ItemLocation = ListIndex * Index * ElementId
  let locListIndex = fun (x, _, _) -> x
  let locIndex = fun (_, x, _) -> x
  let locId = fun (_, _, id) -> id
  
  let previewId id = id + "-preview"

  type SlideDirection =
  | Up
  | Down

  type SlideProgress = {
    StartCoords : Coords
//    EndCoords : Coords
    ElementId : ElementId
    // Direction : SlideDirection
    // Progress : int //# of frames
  } with
    static member Create s ele = {
      StartCoords = s
      //EndCoords = e
      ElementId = ele
      // Direction = dir
      // Progress = p
    }

    //member this.Step() = { this with Progress = this.Progress + 1 }

  type Model = {
    Cursor : Coords
    Offset: Coords option
    Moving : (Index * ElementId) option
    Items: (ItemLocation * ReactElement) list list
    SlideState : SlideProgress option
  }


  let initItemLocations lists =
    lists
    |> List.mapi(fun i li ->
      li
      |> List.mapi (fun j x ->
        (i, j, fst x), snd x
      )
    )

  let Input =
    FunctionComponent.Of(fun (props : {| value : string; id : string |}) ->
      let state = Hooks.useState(props.value)
      input [ 
        Id props.id
        Value props.value
        OnChange (fun v -> state.update(fun s -> s + (string) v.Value ))
        OnMouseEnter (fun e -> e.preventDefault())
        OnMouseDown (fun e -> e.preventDefault())
      ]
    )

  let init() = {
    Cursor = { x = 0. ; y = 0.}
    Offset = None
    Moving = None
    Items = [[
      ("draggable-0", p [] [ str "This is some content" ])
      ("draggable-1", div [] [
            p [] [ str "And some content with an input box" ]
            Input ({|  value = ""; id = "" |})
            input [ Style [ PointerEvents "auto"; Cursor "text" ]; OnMouseDown (fun ev -> ev.preventDefault()) ]
      ])
      ("draggable-2", p [ ] [ str "And yet even more" ])
    ];
    [
      ("draggable-3", p [ ] [ str "This is is also content" ])
      ("draggable-4", p [ ] [ str "And some more content" ])
      ("draggable-5", h3 [ ] [ str "And more content, but different" ])
      ("draggable-6", p [] [ str "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."])
    ]] |> initItemLocations
    SlideState = None
  }

  type Msg =
  | DragStart of loc : ItemLocation * start : Coords * offset : Coords
  | OnDrag of elementId : ElementId * coords : Coords
  | DragOver of loc : ItemLocation// * slideDir : SlideDirection
  | DragEnd
  //| SlideStart of startIndex : Index * endIndex : Index * elementId : ElementId
  // Progress is a % from 0 to 100
  | Slide of sliding : SlideProgress
    //startIndex : Index * endIndex : Index * elementId : ElementId * dir : SlideDirection * progress : int 
  

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
    DOMAttr.OnMouseEnter(fun ev ->
      ev.preventDefault()
      DragOver(loc) |> dispatch
    )
  ]

  let IsDragged {Coords.x = x; y = y} : IHTMLProp list = [
    Style [
      CSSProp.Position PositionOptions.Fixed
      CSSProp.Left (x - 20.)
      CSSProp.Top (y - 20.)
      Opacity 0.8f
      PointerEvents "none"
    ]
  ]

  let IsPreview() : IHTMLProp list = [
    Style [
      Opacity 0.2f
      PointerEvents "none"
    ]
    Id "preview"
  ]

  // todo: the slide locations are slightly off, leading all of it to feel a little janky
  let IsSliding (slide : SlideProgress) : IHTMLProp list =
    let slider = Browser.Dom.document.getElementById(slide.ElementId)
    printfn "sliding id %s is %A" slide.ElementId slider
    if isNull slider then
      [
        Style [
          Position PositionOptions.Absolute
          Left (slide.StartCoords.x - 20. )
          Top (slide.StartCoords.y - 20.)
          CSSProp.TransitionDuration "0.6s"
          CSSProp.TransitionProperty "top"
        ]
      ]
    else
      let rect = slider.getBoundingClientRect()
      [
        Style [
          Position PositionOptions.Absolute
          Left (rect.left - 20.)
          Top (rect.top - 20.)
          CSSProp.TransitionDuration "0.6s"
          CSSProp.TransitionProperty "top"
        ]
      ]


  let draggable model dispatch loc _class content =
    let id = locId loc
    match model.Moving with
    | Some (sourceIndex, elementId) when elementId = id ->
      let coords = model.Cursor -- model.Offset
      div [ ] [
        div [ yield! IsDragged coords; ClassName (_class + " dragged"); Id id ] [ content ]
        div [ Id "preview"; yield! IsPreview(); ClassName (_class);] [content]
      ]
    | Some _ ->
      // if we have an active drag, dont render anything as draggable
      div [ ClassName _class; Id id ] [ content ]
    | None ->
      printfn "rendering normal draggable"
      div [
        yield! IsDraggable loc "test" dispatch; ClassName _class; Id id
      ] [
        h2 [ Style [ Position PositionOptions.Absolute; Left 0; Top 0 ]; ClassName "handle" ] [ str "+" ]
        content
      ]

  let sortedDraggables model dispatch _class (items : (ItemLocation * ReactElement) list) =
    match model.Moving with
    | Some (draggedSourceIndex, draggedElementId) ->
      match model.SlideState with
      | Some slide ->
        /// an element is being dragged and an element is sliding
        items |> List.map (fun (loc, content) ->
          let id = locId loc
          if id = draggedElementId then
            let coords= model.Cursor -- model.Offset
            let hover = div [ yield! IsDragged coords; ClassName (_class + " dragged"); Id id ] [content]
            let preview = div [ yield! IsPreview(); ClassName _class ] [ content ]
            div [] [ hover; preview ]
          elif id = slide.ElementId then
            let slider = 
              div [
                yield! IsSliding slide
                ClassName (_class + " slide")
                Id "slider"
              ] [ content ]
            // "opacity 0.01" lets this element be effectively invisible while taking up space like we need it to
            let element = div [ ClassName _class; Id id; Style [ Opacity 0.01] ] [ content ]
            div [] [
              slider
              element
            ]
          else
            div [ 
              yield! IsListener loc dispatch; ClassName _class; Id id
            ] [ 
              content 
            ]
        )
      | None -> 
        /// an element is being dragged but nothing is sliding
        items |> List.map (fun (loc, content) ->
          let id = locId loc
          if id = draggedElementId then
            let coords= model.Cursor -- model.Offset
            let hover = div [ yield! IsDragged coords; ClassName (_class + " dragged"); Id id ] [content]
            let preview = div [ yield! IsPreview(); ClassName _class ] [ content ]
            div [] [ hover; preview ]
          else
            div [ 
              yield! IsListener loc dispatch; ClassName _class; Id id
            ] [ 
              content 
            ]
        )
    | None ->
      // nothing is being dragged
      items |> List.map (fun (loc, content) ->
        div [ 
          yield! IsDraggable loc "test" dispatch; ClassName _class; Id (locId loc)
          Style [
            Position PositionOptions.Relative
          ]
        ] [
          h2 [ Style [ Position PositionOptions.Absolute; Left 2; Top -22 ]; ClassName "handle" ] [ str "+" ]
          content 
        ]
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
        Hover = hover @ [ Opacity 0.8; Position PositionOptions.Fixed; Margin 0 ]
        Preview = preview @ [ Opacity 0.2 ]
        Area = area
      }

    let SortedDraggable loc dragged (styles : Styles) (dispatch : Msg -> unit) content =
      let id = locId loc
      match dragged with
      | Some (_, dei) when dei = id ->
        // draw the regular element, with the preview styles
        let preview = div [ Style styles.Preview ] content
        let hover = div [ Style styles.Hover ] content
        div [] [
          preview
          hover
        ]
      | Some (_, _) ->
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
    let DropArea (id, _type, _class, dragged : (Index * ElementId) option, dispatch, contents) = 
      div [ 
        Id id
        ClassName ("drop-area " + _class)
        Style [
          Custom ("dropareaType", _type)
          Height "200px"
          if dragged.IsSome then CSSProp.Cursor "grabbing"
        ]
        match dragged with
        | Some (sourceIndex, eleId) ->
          OnMouseMove(fun ev ->
            ev.preventDefault()
            let c = coords ev.clientX ev.clientY
            OnDrag (eleId, c) |> dispatch
          )
          OnMouseUp (fun ev -> ev.preventDefault();  DragEnd |> dispatch)
        | None -> ()
      ] contents

  let dropArea model dispatch = 
    let left = 
      Components.DropArea("left-container", "test", "container left", model.Moving, dispatch, 
        sortedDraggables model dispatch "content" model.Items.[0]
      )

    let right =
      let items = 
        model.Items.[1]
        |> sortedDraggables model dispatch "content"
      Components.DropArea("right-container", "test", "container right", model.Moving, dispatch, items)

    div [ ] [
        left
        right
    ]

  let view model dispatch =
    div [] [
        h2 [] [ str "Drag and drop 2 dropping boogaloo"]
        div [ ClassName "wrapper" ] [
            dropArea model dispatch
        ]
    ]

  let private split i li =
    let len x = List.length x
    let first = if len li >= i then List.take i li else li
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
    let previewId = previewId id
    let isTarget (eleId, _) =
      if (locId eleId) = id then
        true
      elif (locId eleId) = previewId then
        true
      else
        false

    let filterList li =
      li
      |> List.filter (fun x -> not(isTarget x))

    let updatedLists =
      model.Items
      |> List.map(fun li -> filterList li)
    updatedLists

  let private insertAt (listIndex, index, id) item (li: (ItemLocation * ReactElement) list) =
    //printfn "Inserting %s to list %i at index %i" id listIndex index
    let toIds li = li |> List.map (fun ((_, _, x), _) -> x)
    printfn "list is %A" (toIds li)
    let li = 
      if index = 0 then
        //let h = List.tryHead li
        ((listIndex, 0, id), item) :: li
      else
        let h, t = split index li
        //let slider = List.tryHead t
        h @ (((listIndex, index, id), item) :: t)
    li |> List.mapi (fun i ((a, _, b), v ) -> ((a, i, b), v))

  let private replaceListAtIndex listIndex li items =
    items
    |> List.mapi (fun i x -> if i = listIndex then li else x)

  let private moveItem (listIndex, index, oldElementId) (_, newElementId) model =
    let removedElementOpt = getItemContent newElementId model
    let allItems = removeElement newElementId model
    match removedElementOpt with
    | Some ele ->
      let newList = insertAt (listIndex, index, newElementId) (ele) allItems.[listIndex]
      let newItems = replaceListAtIndex listIndex newList allItems
      match model.Moving with
      | Some (sourceIndex, _) ->
        let oldelement = Browser.Dom.document.getElementById(oldElementId)
        //let preview = Browser.Dom.document.getElementById("preview")
        let startCoords = coords oldelement.offsetLeft oldelement.offsetTop
        //let endCoords = coords preview.offsetLeft preview.offsetTop
        let msg = SlideProgress.Create startCoords oldElementId |> Slide
        { model with Items = newItems }, Cmd.ofMsg msg
      | None ->
        printfn "unreachable state: moving an item but no dragged item found"
        { model with Items = newItems }, Cmd.none
    | None ->
      { model with Items = allItems }, Cmd.none

  let update msg model =
    match msg with
    | DragStart (loc, startCoords, offset) ->
      { model with Moving = Some (locIndex loc, locId loc); Cursor = startCoords; Offset = Some offset }, Cmd.none
    | OnDrag (element, coords) ->
      {model with Cursor = coords }, Cmd.none
    | DragOver (loc) ->
      match model.Moving with
      | Some id ->
        moveItem loc id model
      | None ->
        model, Cmd.none
    | Slide sliding ->
      { model with SlideState = Some sliding}, Cmd.none
    | DragEnd ->
      { model with Moving = None; SlideState = None }, Cmd.none
