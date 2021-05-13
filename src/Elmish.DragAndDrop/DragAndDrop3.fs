namespace Elmish

module List =
  open System

  let private len x = List.length x

  let tryTake n li =
    if len li < n then
      li
    elif n < 0 then
      []
    else
      List.take n li

  let removeAt index li =
    if len li < index then
      li
    elif index = 0 then
      List.tail li
    else
      let h = tryTake (index) li
      let t = List.skip (index + 1) li
      h @ t

  let insertAt item index li =
    if len li <= index then
      li @ [item]
    elif index <= 0 then
      item :: li
    else
      let h = tryTake (index) li
      let t = List.skip (index) li
      h @ [item] @ t


  let replaceAt item index li =
    if len li <= index then
      li @ [ item ]
    elif index = 0 then
      let t = List.tail li
      item :: t
    else
      let h = tryTake (index) li
      let t = List.skip (index + 1) li
      h @ [item] @ t

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
      //ElementId : ElementId
      StartLocation : ItemLocation
    } with
        member this.setSlide (slide : Slide option) = { this with Slide = slide }
        static member Init loc = { Slide = None; StartLocation = loc }
          //ElementId = id }
  
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

    member this.ElementIds() =
      this.Items
      |> List.map (fun itemList ->
        itemList |> List.map (fun (_, _, id) -> string id)
      )


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

    let updateItemLocations (items : ItemLocation list list) =
      items
      |> List.mapi (fun i itemList ->
        itemList
        |> List.mapi (fun j (_, _, id) ->
          (i, j, id)
        )
      )

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
  //| DraggedListener of (Browser.Types.MouseEvent -> unit)
  | HoverListener of (Browser.Types.MouseEvent -> unit)

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

    // let draggableWithFunc model elementId func dispatch =
    //   let loc = getLocationForElement elementId model
    //   match loc with
    //   | Some loc ->
    //     (fun (ev : MouseEvent) ->
    //       ev.preventDefault()
    //       func ev
    //       let o = Helpers.getOffset ev (locId loc)
    //       (loc, fromME ev, o) |> DragStart |> dispatch
    //     )
    //   | None -> (fun ev -> func ev; ())

    let defaultMouseMoveListener dispatch =
      (fun (ev : MouseEvent) ->
        ev.preventDefault()
        let c = coords ev.clientX ev.clientY
        OnDrag c |> dispatch
      )

    // let mouseMoveListenerWithFunc func dispatch =
    //   (fun (ev : MouseEvent) ->
    //     ev.preventDefault()
    //     func ev
    //     let c = coords ev.clientX ev.clientY
    //     OnDrag c |> dispatch
    //   )

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

    // let hoverListenerWithFunc model id func dispatch =
    //   (fun (ev : MouseEvent) ->
    //     ev.preventDefault()
    //     func ev
    //     let loc = getLocationForElement id model
    //     match loc with
    //     | Some loc ->
    //       DragOver loc |> dispatch
    //     | None -> ()
    //   )

    let defaultReleaseListener dispatch =
      (fun (ev : MouseEvent) -> ev.preventDefault();  DragEnd |> dispatch)

  type DropAreaProp =
  | AreaStyle of CSSProp list
  | AreaProps of IHTMLProp list
  | ReleaseListener of (Browser.Types.MouseEvent -> unit)
  | MouseMoveListener of (Browser.Types.MouseEvent -> unit)
  //| DraggedListener of (Browser.Types.MouseEvent -> unit)

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
        // | DragProp.DraggedListener l ->
        //   let l = (OnMouseMove l) :> IHTMLProp
        //   (styles, l :: props)
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
          | MouseMoveListener l ->
            let l = (OnMouseMove l) :> IHTMLProp
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
      | Some { StartLocation = (startList, startIndex, elementId); Slide = Some slide } ->
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
      | Some { StartLocation = (startList, startIndex, elementId); Slide = None } ->
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
    printfn "Inserting %s to list %i at index %i" id listIndex index
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

  let moveItem (listIndex, index, oldElementId) newElementId model =
    let allItems = removeElement newElementId model
    let newList = insertAt (listIndex, index, newElementId) allItems.[listIndex]
    let newItems = replaceListAtIndex listIndex newList allItems
    match model.Moving with
    | Some { StartLocation = (startList, startIndex, x) }  ->
      let oldelement = Browser.Dom.document.getElementById(oldElementId)
      let startCoords = coords oldelement.offsetLeft oldelement.offsetTop
      let slide = Slide.Create startCoords oldElementId
      let m = { model with Items = newItems; } |> Model.buildItemDict
      let m = m.setSlide slide
      m, Cmd.none
    | None ->
      printfn "unreachable state: moving an item but no dragged item found"
      { model with Items = newItems }, Cmd.none

  let private moveItemSameList listIndex startIndex insertAtIndex li =
    match List.tryItem listIndex li with
    | Some innerList ->
      // if we're inserting an item at a later point in the list...
      if startIndex < insertAtIndex then
        // grab everything up to the start
        let beginning, rest = split startIndex innerList
        // grab the middle & end sections
        let middle, _end = split (insertAtIndex - startIndex + 1) rest
        // this middle section should be 2 items - split it & swap them
        let x, y = split 1 middle
        let newList = beginning @ y @ x @ _end
        List.replaceAt newList listIndex li
      // otherwise we're inserting at an earlier point in the list...
      elif startIndex > insertAtIndex then
        // grab everything up to the start
        let beginning, rest = split insertAtIndex innerList
        // grab the end section
        let middle, _end = split (startIndex - insertAtIndex) rest
        let head, tail = split 1 _end
        let newList = beginning @ head @ middle @ tail
        List.replaceAt newList listIndex li
      else
        li
    | None ->
      JS.console.error("Unreachable state: cannot find list at index", listIndex)
      li

  let moveItem2 (startListIndex, startIndex) (insertListIndex, insertAtIndex) li =
    let len x = List.length x
    let split i li = 
      let first =
        if len li > i then List.take i li else li
      let second =
        if len li <= i then [] else  List.skip i li
      first,second

    if startListIndex = insertListIndex then
      moveItemSameList startListIndex startIndex insertAtIndex li
    else
      // lists are not the same; grab the item, insert it to the new list, and remove it from the old list
      match List.tryItem startListIndex li, List.tryItem insertListIndex li with
      | Some startList, Some insertList ->
        match List.tryItem startIndex startList with
        | Some item ->
          let newStartList = List.removeAt startIndex startList
          let newInsertList = List.insertAt item insertAtIndex insertList

          li
          |> List.replaceAt newStartList startListIndex
          |> List.replaceAt newInsertList insertListIndex
        | None ->
          JS.console.error("Unreachable state: cannot find item in list at index", startListIndex, startIndex)
          li
      | None, _ ->
        JS.console.error("Unreachable state: cannot find list at index", startListIndex)
        li
      | _, None ->
        JS.console.error("Unreachable state: cannot find list at index", insertListIndex)
        li

  let update msg model =
    match msg with
    | DragStart (loc, startCoords, offset) ->
      //{ model with Moving = Some (locIndex loc, locId loc), None ; Cursor = startCoords; Offset = Some offset }, Cmd.none
      let movingStatus = MovingStatus.Init (loc) |> Some
      { model with Moving = movingStatus; Cursor = startCoords; Offset = Some offset }, Cmd.none
    | OnDrag coords ->
      {model with Cursor = coords }, Cmd.none
    | DragOver (listIndex, index, elementId) ->
      match model.Moving with
      | Some { StartLocation = (startList, startIndex, startingElementId) }->
        printfn "moving %A over %A" (startList, startIndex, startingElementId) (listIndex, index, elementId)
        // moveItem loc id model
        let items' =
          moveItem2 (startList, startIndex) (listIndex, index) model.Items
          |> Model.updateItemLocations
        printfn "updated items are %A" items'
        let mdl = { model with Items = items' } |> Model.buildItemDict
        mdl, Cmd.none
      | None ->
        model, Cmd.none
    | DragEnd ->
      { model with Moving = None; Offset = None }, Cmd.none