namespace Elmish

module DragAndDrop3 =
  open Elmish
  open Feliz
  open Fable.React
  open Fable.React.Props
  open Fable.Core
  open Fable.Core.JsInterop
  open System.ComponentModel

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
    let createLoc a b c = (a, b, c)

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

    type MovingStatus = {
      Slide : Slide option
      ElementId : ElementId
    } with
        member this.setSlide (slide : Slide option) = { this with Slide = slide }
        static member Init id = { Slide = None; ElementId = id }
  
  open HelperTypes

  type Model = {
    /// The cursor's current coordinates, updated when dragging to draw the ghost.
    Cursor : Coords
    /// The index of the currently moving item (or, rather, the point it is hovering over), and that element's id
    /// The slide contains the starting coordinates & element that is sliding.
    Moving : MovingStatus option
      //(Index * ElementId) option * Slide option
    LocationDict : Map<ElementId, ItemLocation>
    /// The amount to adjust the drag ghost so that it is correctly placed under the cursor.
    Offset : Coords option
    /// The items to be sorted. If there are multiple containers, use multiple lists.
    /// // ReactElement
    Items : (ItemLocation) list list
  } with
    member this.setSlideOpt (so : Slide option) = 
      match this.Moving with
      | Some moving -> { this with Moving = moving.setSlide (so) |> Some }
      | None -> this

    member this.setSlide (s : Slide) =
      match this.Moving with
      | Some moving -> { this with Moving = moving.setSlide (Some s) |> Some }
      | None -> this


  /// Create an empty Model instance
  let empty() = {
    Cursor = { x = 0. ; y = 0.}
    Moving = None
    LocationDict = Map.empty
    Offset = None
    Items = []
  }

  let private initItemLocations lists =
    lists
    |> List.mapi(fun i li ->
      li
      |> List.mapi (fun j x ->
        (i, j, x)
      )
    )

  module Model =

    let buildItemDict (model : Model) =
      let d = 
        model.Items
        |> List.map(fun li ->
          li 
          |> List.map (fun (listInd, ind, id) ->
            id, (listInd, ind, id)
          )
        )
        |> List.concat
        |> Map.ofList
      { model with LocationDict = d }

    let createWithItemsMultiList (items : ElementId list list) =
      let itemsWithLocs = initItemLocations items
      { empty() with Items = itemsWithLocs } |> buildItemDict

    let createWithItems (items : ElementId list) =
      let itemsWithLocs = initItemLocations [items]
      { empty() with Items = itemsWithLocs } |> buildItemDict


  let getLocationForElement elementId model = 
    model.Items
    |> List.map (fun li -> li |> List.tryFind (fun (_, _, id) -> id = elementId))
    |> List.choose id
    |> List.tryHead

  type Msg =
  /// Indicates dragging has started on an element. Requires the item location and starting coordinates.
  | DragStart of loc : ItemLocation * start : Coords * offset : Coords
  /// An item is currently being dragged. This updates the location of the cursor.
  | OnDrag of coords : Coords
  | DragOver of loc : ItemLocation
  | DragEnd

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


  module Styles =

    /// `IHTMLProp` properties for an item that is currently being dragged.
    let IsDragged model : CSSProp list =
      let coords = model.Cursor
      [
        CSSProp.Cursor "grabbing"
        CSSProp.Position PositionOptions.Fixed
        CSSProp.Left coords.x
        CSSProp.Top coords.y
        Opacity 0.8f
      ]

    /// `IHtmlProp` properties for an item that is able to be dragged
    let IsDraggable () : CSSProp list =
      [
        CSSProp.Custom ("draggable", true)
        CSSProp.Cursor "grab"
      ]
    

    /// `IHTMLProp` properties for an item that is a preview of where a drop will go
    let IsPreview() : CSSProp list =
      [
        Opacity 0.2f
        PointerEvents "none"
      ]


  type DragProp =
  | DraggedStyle of CSSProp list
  | DraggableStyle of CSSProp list
  | PreviewStyle of CSSProp list
  | SlideStyle of CSSProp list
  | DraggedProps of IHTMLProp list
  | DraggableProps of IHTMLProp list
  | PreviewProps of IHTMLProp list
  | SlideProps of IHTMLProp list
  | DraggableListener of (Browser.Types.MouseEvent -> unit)
  | DraggedListener of (Browser.Types.MouseEvent -> unit)
  | HoverListener of (Browser.Types.MouseEvent -> unit)
  // | DropListener of (Browser.Types.MouseEvent -> unit)

  module Listeners =
    open Browser.Types

    let defaultDraggable model elementId dispatch =
      let loc = getLocationForElement elementId model
      match loc with
      | Some loc ->
        fun (ev : Browser.Types.MouseEvent) ->
          ev.preventDefault()
          let o = Helpers.getOffset ev (locId loc)
          (loc, fromME ev, o) |> DragStart |> dispatch
      | None -> 
        printfn "no location found for element %s" elementId
        fun (ev : Browser.Types.MouseEvent) -> ()

    let draggableWithFunc model elementId func dispatch =
      let loc = getLocationForElement elementId model
      match loc with
      | Some loc ->
        (fun (ev : MouseEvent) ->
          ev.preventDefault()
          func ev
          let o = Helpers.getOffset ev (locId loc)
          (loc, fromME ev, o) |> DragStart |> dispatch
        )
      | None -> (fun ev -> func ev; ())

    let defaultDragListener dispatch =
      (fun (ev : MouseEvent) ->
        ev.preventDefault()
        let c = coords ev.clientX ev.clientY
        OnDrag c |> dispatch
      )

    let dragListenerWithFunc func dispatch =
      (fun (ev : MouseEvent) ->
        ev.preventDefault()
        func ev
        let c = coords ev.clientX ev.clientY
        OnDrag c |> dispatch
      )

    /// Listener for when another element is being dragged and is moved over this element.
    let defaultHoverListener model id dispatch =
      (fun (ev : MouseEvent) ->
        ev.preventDefault()
        let loc = getLocationForElement id model
        match loc with
        | Some loc ->
          DragOver loc |> dispatch
        | None -> ()
      )

    let hoverListenerWithFunc model id func dispatch =
      (fun (ev : MouseEvent) ->
        ev.preventDefault()
        func ev
        let loc = getLocationForElement id model
        match loc with
        | Some loc ->
          DragOver loc |> dispatch
        | None -> ()
      )

    let defaultReleaseListener dispatch =
      (fun (ev : MouseEvent) -> ev.preventDefault();  DragEnd |> dispatch)

  type DropAreaProp =
  | AreaStyle of CSSProp list
  | AreaProps of IHTMLProp list
  | ReleaseListener of (Browser.Types.MouseEvent -> unit)
  // | DraggedListener of (Browser.Types.MouseEvent -> unit)

  module PropertyFolding =

    type FoldState = CSSProp list * IHTMLProp list
    let foldStateZero = ([], [])

    let foldWithMatcher (props: DragProp list) matcher : IHTMLProp list =
      let styles, htmlProps =
        props
        |> List.fold(fun (styles, htmlProps) x -> matcher (styles, htmlProps) x 
        ) foldStateZero
      [
        yield Style styles
        yield! htmlProps
      ]

    let foldDraggable (dragProps: DragProp list) : IHTMLProp list =
      let matcher (styles, properties) x =
        match x with
        | DraggableStyle s -> (s @ styles, properties)
        | DraggableProps p -> (styles, p @ properties)
        | DraggableListener func ->
          let h = (OnMouseDown func) :> IHTMLProp
          (styles, h :: properties)
        | _ -> (styles, properties)
      foldWithMatcher dragProps matcher

    let foldDragged (dragProps : DragProp list) : IHTMLProp list =
      let matcher (styles, props) x =
        match x with
        | DraggedStyle s -> (s @ styles, props)
        | DraggedProps p -> (styles, p @ props)
        | DraggedListener l ->
          let l = (OnMouseMove l) :> IHTMLProp
          (styles, l :: props)
        | _ -> (styles, props)
      foldWithMatcher dragProps matcher

    let foldPreview (dragProps : DragProp list) : IHTMLProp list =
      let matcher (styles, props) x =
        match x with
        | PreviewStyle s -> (s @ styles, props)
        | PreviewProps p -> (styles, p @ props)
        | _ -> (styles, props)
      foldWithMatcher dragProps matcher

    let foldSlide elementId (dragProps : DragProp list) : IHTMLProp list =
      let slider = Browser.Dom.document.getElementById(elementId)
      if isNull slider then
        let matcher (styles, props) x =
          match x with
          | SlideStyle s -> (s @ styles, props)
          | SlideProps p -> (styles, p @ props)
          | _ -> (styles, props)
        foldWithMatcher dragProps matcher
      else
        let rect = slider.getBoundingClientRect()
        let location =
          [
            Left rect.left
            Top rect.top
          ] |> SlideStyle
        let matcher (styles, props) x =
          match x with
          | SlideStyle s -> (s @ styles, props)
          | SlideProps p -> (styles, p @ props)
          | _ -> (styles, props)
        foldWithMatcher (location :: dragProps) matcher

    // called when something is being dragged but it's not this element
    let foldDraggableDuringDrag (dragProps : DragProp list) : IHTMLProp list =
      let matcher (styles, properties) x =
        match x with
        | DraggableStyle s -> (s @ styles, properties)
        | DraggableProps p -> (styles, p @ properties)
        | HoverListener hl ->
          let hl = (OnMouseEnter hl) :> IHTMLProp
          (styles, hl :: properties)
        | _ -> (styles, properties)
      foldWithMatcher dragProps matcher

    let foldDropArea props : IHTMLProp list =
      let styles, htmlProps =
        props
        |> List.fold(fun (styles, htmlProps) x ->
          match x with
          | AreaStyle s -> (s @ styles, htmlProps)
          | AreaProps p -> (styles, p @ htmlProps)
          | ReleaseListener l ->
            let l = (OnMouseUp l) :> IHTMLProp
            (styles, l :: htmlProps)
        ) foldStateZero
      [
        yield Style styles
        yield! htmlProps
      ]

    let foldDropAreaWithoutListeners props : IHTMLProp list =
      let styles, htmlProps =
        props
        |> List.fold(fun (styles, htmlProps) x ->
          match x with
          | AreaStyle s -> (s @ styles, htmlProps)
          | AreaProps p -> (styles, p @ htmlProps)
          | _ -> (styles, htmlProps)
        ) foldStateZero
      [
        yield Style styles
        yield! htmlProps
      ]

  open PropertyFolding

  /// An element that is designed to be dragged
  type Draggable =
    //static member inline handle (ele : ReactElement) = ()
    
    /// Renders a draggable item with the appropriate listeners based on the state of the model
    /// Will also rending a drop preview and/or a sliding element as needed.
    static member inline draggable model id (props : DragProp list) children =
      match model.Moving with
      | None ->
        let htmlProps = foldDraggable props
        div htmlProps children
      | Some { ElementId = elementId; Slide = Some slide } ->
        // something is being dragged and something is sliding
        if id = elementId then
          // this is the dragged item; render it & the preview
          let element = div (foldDragged props) children
          let preview = div (foldPreview props) children
          div [] [ element; preview ]
        elif id = slide.ElementId then
          printfn "id is %A, slide is %A" id slide
          // this is the sliding item; render it & the reference
          let slider = div (foldSlide id  props) children
          let placeHolderProps = [ DraggableStyle [ Opacity 0.001 ]; SlideProps [ Id id ]] @ props
          let placeholder = div (foldDraggable placeHolderProps) children
          div [] [ slider; placeholder ]
        else
          // there is an active drag, but it's not this item
          div (foldDraggableDuringDrag props) children
      | Some { ElementId = elementId; Slide = None } ->
        // there is a drag, but nothing is sliding
        if id = elementId then
          // this is the dragged item; render it & the preview
          let element = div (foldDragged props) children
          let preview = div (foldPreview props) children
          div [] [ element; preview ]
        else
          // there is an active drag, but it's not this item
          div (foldDraggableDuringDrag props) children


  type DropArea =

    static member inline dropArea model (props : DropAreaProp list) children =
      match model.Moving with
      | None ->
        let htmlProps = foldDropAreaWithoutListeners props
        div htmlProps children
      | Some _ ->
        let htmlProps = foldDropArea props
        div htmlProps children


  let private split i li =
    let len x = List.length x
    let first = if len li >= i then List.take i li else li
    let second = if len li <= i then [] else List.skip i li
    first, second


  let private removeElement id model =
    let previewId = id + "-preview"
    let isTarget (eleId) =
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

  let private insertAt (listIndex, index, id) (li: ItemLocation list) =
    //printfn "Inserting %s to list %i at index %i" id listIndex index
    let toIds li = li |> List.map (fun (_, _, x) -> x)
    printfn "list is %A" (toIds li)
    let li = 
      if index = 0 then
        (listIndex, 0, id) :: li
      else
        let h, t = split index li
        h @ ((listIndex, index, id) :: t)
    li |> List.mapi (fun i (a, _, b) -> (a, i, b))

  let private replaceListAtIndex listIndex li items =
    items
    |> List.mapi (fun i x -> if i = listIndex then li else x)

  let private moveItem (listIndex, index, oldElementId) newElementId model =
    let allItems = removeElement newElementId model
    let newList = insertAt (listIndex, index, newElementId) allItems.[listIndex]
    let newItems = replaceListAtIndex listIndex newList allItems
    match model.Moving with
    | Some { ElementId = x } ->
      let oldelement = Browser.Dom.document.getElementById(oldElementId)
      let startCoords = coords oldelement.offsetLeft oldelement.offsetTop
      let slide = Slide.Create startCoords oldElementId
      let m = { model with Items = newItems; } |> Model.buildItemDict
      let m = m.setSlide slide
      m, Cmd.none
    | None ->
      printfn "unreachable state: moving an item but no dragged item found"
      { model with Items = newItems }, Cmd.none

  let update msg model =
    match msg with
    | DragStart (loc, startCoords, offset) ->
      //{ model with Moving = Some (locIndex loc, locId loc), None ; Cursor = startCoords; Offset = Some offset }, Cmd.none
      let movingStatus = MovingStatus.Init (locId loc) |> Some
      { model with Moving = movingStatus; Cursor = startCoords; Offset = Some offset }, Cmd.none
    | OnDrag coords ->
      {model with Cursor = coords }, Cmd.none
    | DragOver (loc) ->
      match model.Moving with
      | Some { ElementId = id } ->
        moveItem loc id model
      | None ->
        model, Cmd.none
    | DragEnd ->
      { model with Moving = None; Offset = None }, Cmd.none