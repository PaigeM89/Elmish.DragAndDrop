namespace Elmish

module DragAndDrop2 =

  open System
  open Elmish
  open Feliz
  open Fable.React
  open Fable.React.Props

  module HelperTypes =
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
    let fromME (ev : Browser.Types.MouseEvent) = { x = ev.clientX; y = ev.clientY }

    type ListIndex = int
    type Index = int
    type ItemLocation = ListIndex * Index * ElementId
    let locListIndex = fun (x, _, _) -> x
    let locIndex = fun (_, x, _) -> x
    let locId = fun (_, _, id) -> id

    type Slide  = {
      /// The starting coordinates of the element that is sliding
      StartCoords : Coords
      /// The ID of the element that is sliding
      ElementId : ElementId
    } with
      static member Create s ele = {
        StartCoords = s
        ElementId = ele
      }

  open HelperTypes

  type Model = {
    /// The cursor's current coordinates, updated when dragging to draw the ghost.
    Cursor : Coords
    /// The index of the currently moving item (or, rather, the point it is hovering over), and that element's id
    /// The slide contains the starting coordinates & element that is sliding.
    Moving : (Index * ElementId) option * Slide option
    /// The amount to adjust the drag ghost so that it is correctly placed under the cursor.
    Offset : Coords option
    /// The items to be sorted. If there are multiple containers, use multiple lists.
    Items : (ItemLocation * ReactElement) list list
  }

  /// Create a new Model instance
  let init() = {
    Cursor = { x = 0. ; y = 0.}
    Moving = None, None
    Offset = None
    Items = []
  }

  type Msg =
  /// Indicates dragging has started on an element. Requires the item location and starting coordinates.
  | DragStart of loc : ItemLocation * start : Coords * offset : Coords
  /// An item is currently being dragged. This updates the location of the cursor.
  | OnDrag of coords : Coords
  | DragOver of loc : ItemLocation
  | DragEnd

  let private initItemLocations lists =
    lists
    |> List.mapi(fun i li ->
      li
      |> List.mapi (fun j x ->
        (i, j, fst x), snd x
      )
    )

  /// this needs heavy refactoring once i can figure out a design approach
  /// this is also serving as a storage for how "general" dragging works (no dropping yet)
  /// while the example page is worked on 

  module private Helpers =
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

  // todo: categories later
  // let DraggableType (t : string) = CSSProp.Custom ("draggableType", t)
  // let IsDropArea (t : string) = CSSProp.Custom ("drop-bucket", t)

  /// `IHTMLProp` properties for items that are draggable.
  let IsDraggable loc dispatch : IHTMLProp list= [
    Style [
      CSSProp.Cursor "grab"
    ]
    DOMAttr.OnMouseDown(fun ev ->
      ev.preventDefault()
      let o = Helpers.getOffset ev (locId loc)
      (loc, fromME ev, o) |> DragStart |> dispatch
    )
  ]

  /// Mouse Enter listener to know when an item may have to move or shift to accomdate a drop.
  let IsListener loc dispatch : IHTMLProp list = [
    DOMAttr.OnMouseEnter(fun ev ->
      ev.preventDefault()
      DragOver(loc) |> dispatch
    )
  ]

  /// `IHTMLProp` properties for an item that is currently being dragged.
  let IsDragged {Coords.x = x; y = y} : IHTMLProp list = [
    Style [
      CSSProp.Cursor "grabbing"
      CSSProp.Position PositionOptions.Fixed
      CSSProp.Left x
      CSSProp.Top y
      Opacity 0.8f
    ]
  ]

  /// `IHTMLProp` properties for an item that is a preview of where a drop will go
  let IsPreview() : IHTMLProp list = [
    Style [
      Opacity 0.2f
      PointerEvents "none"
    ]
    Id "preview"
  ]

  /// `ITHMLProp` properties for an item sliding out of the way of another item, using CSS to slide.
  // todo: the slide locations are slightly off, leading all of it to feel a little janky
  let IsSliding (slide : Slide) : IHTMLProp list =
    let slider = Browser.Dom.document.getElementById(slide.ElementId)
    // the first time this is called is to generate the slider properties, 
    // so when this is null we're initializing the slider.
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
    | Some (_, elementId), _ when elementId = id ->
      let coords = model.Cursor -- model.Offset
      div [ ] [
        div [ yield! IsDragged coords; ClassName (_class + " dragged"); Id id ] [ content ]
        div [ Id (id + "-preview"); yield! IsPreview(); ClassName (_class);] [content]
      ]
    | Some _, _->
      // if we have an active drag, dont render anything as draggable
      div [ ClassName _class; Id id ] [ content ]
    | None, _ ->
      printfn "rendering normal draggable"
      div [
        yield! IsDraggable loc dispatch; ClassName _class; Id id
      ] [
        h2 [ Style [ Position PositionOptions.Absolute; Left 0; Top 0 ]; ClassName "handle" ] [ str "+" ]
        content
      ]


  let sortedDraggables model dispatch _class (items : (ItemLocation * ReactElement) list) =
    match model.Moving with
    | Some (draggedSourceIndex, draggedElementId), slide ->
      match slide with
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
    | None, _ ->
      // nothing is being dragged
      items |> List.map (fun (loc, content) ->
        div [ 
          yield! IsDraggable loc dispatch; ClassName _class; Id (locId loc)
          Style [
            Position PositionOptions.Relative
          ]
        ] [
          h2 [ Style [ Position PositionOptions.Absolute; Left 2; Top -22 ]; ClassName "handle" ] [ str "+" ]
          content 
        ]
      )

  module Components =
    open Fable.Core
    open Feliz

    [<AutoOpen>]
    module Draggable =
      type IDraggableAttribute = interface end

      [<Erase>]
      type Draggable() =
        static member inline draggable props contents =
          div props contents

      type draggable =
        static member inline id (value: string) = unbox<IDraggableAttribute>("id", value)
        static member inline _class (value : string) = unbox<IDraggableAttribute>("class", value)
        static member inline draggableType (value : string) = unbox<IDraggableAttribute>("draggableType", value)
        static member inline x (x : int) = unbox<IDraggableAttribute>("")

    let SortedDropArea id draggedElementId (dispatch : Msg -> unit) content =
      match draggedElementId with
      | Some dei ->
        div [
          Id id
          OnMouseUp (fun _ -> DragEnd |> dispatch)
          OnMouseMove (fun ev -> fromME ev |> OnDrag |> dispatch )
          // todo: supply this some other way
          ClassName "drop-area"
        ] content
      | None ->
        div [
          Id id
          //Style styles.Area
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
        | Some _ ->
          OnMouseMove(fun ev ->
            ev.preventDefault()
            let c = coords ev.clientX ev.clientY
            OnDrag c |> dispatch
          )
          OnMouseUp (fun ev -> ev.preventDefault();  DragEnd |> dispatch)
        | None -> ()
      ] contents

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
    let previewId = id + "-preview"
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
      | Some (sourceIndex, movingElementId), _ ->
        let oldelement = Browser.Dom.document.getElementById(oldElementId)
        let startCoords = coords oldelement.offsetLeft oldelement.offsetTop
        let slide = Slide.Create startCoords oldElementId |> Some
        let m = (sourceIndex, movingElementId) |> Some
        { model with Items = newItems; Moving = (m, slide) }, Cmd.none
      | None, _ ->
        printfn "unreachable state: moving an item but no dragged item found"
        { model with Items = newItems }, Cmd.none
    | None ->
      { model with Items = allItems }, Cmd.none

  let update msg model =
    match msg with
    | DragStart (loc, startCoords, offset) ->
      { model with Moving = Some (locIndex loc, locId loc), None ; Cursor = startCoords; Offset = Some offset }, Cmd.none
    | OnDrag coords ->      { model with Cursor = coords }, Cmd.none
    | DragOver (loc) ->
      match model.Moving with
      | Some id, _ ->
        moveItem loc id model
      | _ ->
        model, Cmd.none
    | DragEnd ->
      { model with Moving = None, None; Offset = None }, Cmd.none
