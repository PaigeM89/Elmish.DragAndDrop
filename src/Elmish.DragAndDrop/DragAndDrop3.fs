namespace Elmish

open DragAndDrop

module DragAndDrop3 =
  open Elmish
  open Feliz
  open Fable.React
  open Fable.React.Props
  open Fable.Core
  open Fable.Core.JsInterop
  open System.ComponentModel

  // TODO:
  // * can't insert to end of another list unless you drag to inner part of the list, then down
  // * Sliding doesn't work at all.
  // * Create "content templates" for a drag area, so you don't have to recreate so much per item

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


    let tryGetElementCoords elementId = 
      let element = Browser.Dom.document.getElementById(elementId)
      if isNull element then
        None
      else
        coords element.offsetLeft element.offsetTop
        |> Some

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
    LocationDict : Map<ElementId, ItemLocation>
    /// The amount to adjust the drag ghost so that it is correctly placed under the cursor.
    Offset : Coords option
    /// The items to be sorted. If there are multiple containers, use multiple lists.
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

  module ModelFuncs =

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

    let getItemLocation id model = Map.tryFind id model.LocationDict

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

    let setDragSource loc model =
      match model.Moving with
      | Some { Slide = Some slide} ->
        let moving = {MovingStatus.Init loc  with Slide = Some slide }
        { model with Moving = Some moving }
      | Some { Slide = None } ->
        let moving = MovingStatus.Init loc
        { model with Moving = Some moving }
      | None ->
        let moving = MovingStatus.Init loc
        { model with Moving = Some moving }

    let setSlide slide (model : Model) = model.setSlide slide
    let setSlideOpt slide (model : Model) = model.setSlideOpt slide

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
  //| SlideStart of slidingElementId : ElementId
  | DragEnd

  module private Helpers =
    let getDraggedElement id =
      printfn "getting dragged element %s" id
      let doc = Browser.Dom.document
      let ele = doc.getElementById(id)
      ele
    let getOffset ev id =
      printfn "getting offset for element %s" id
      let ele = getDraggedElement id
      let rect = ele.getBoundingClientRect()
      let coords = fromME ev
      let x = coords.x - rect.left
      let y = coords.y - rect.top
      { x = x; y = y}


  // module Styles =

  //   /// `IHTMLProp` properties for an item that is currently being dragged.
  //   let IsDragged model : CSSProp list =
  //     let coords = model.Cursor
  //     [
  //       CSSProp.Cursor "grabbing"
  //       CSSProp.Position PositionOptions.Fixed
  //       CSSProp.Left coords.x
  //       CSSProp.Top coords.y
  //       Opacity 0.8f
  //     ]

  //   /// `IHtmlProp` properties for an item that is able to be dragged
  //   let IsDraggable () : CSSProp list =
  //     [
  //       CSSProp.Custom ("draggable", true)
  //       CSSProp.Cursor "grab"
  //     ]
    

  //   /// `IHTMLProp` properties for an item that is a preview of where a drop will go
  //   let IsPreview() : CSSProp list =
  //     [
  //       Opacity 0.2f
  //       PointerEvents "none"
  //     ]

  // type SlideProp =
  // | SlideStyle of CSSProp list
  // | SlideProps of IHTMLProp list
  // | LeftOffset of float
  // | TopOffset of float
  // // how long the slide animation should take, in seconds
  // | TransitionDuration of float

  // type DragProp =
  // | DraggedStyle of CSSProp list
  // | DraggableStyle of CSSProp list
  // | PreviewStyle of CSSProp list
  // //| SlideStyle of CSSProp list
  // | DraggedProps of IHTMLProp list
  // | DraggableProps of IHTMLProp list
  // | PreviewProps of IHTMLProp list
  // //| SlideProps of IHTMLProp list

  module internal Listeners =
    open Browser.Types

    let defaultDraggable model elementId dispatch =
      let loc = getLocationForElement elementId model
      match loc with
      | Some loc ->
        OnMouseDown (fun (ev : Browser.Types.MouseEvent) ->
          ev.preventDefault()
          let o = Helpers.getOffset ev (locId loc)
          (loc, fromME ev, o) |> DragStart |> dispatch
        )
      | None -> 
        printfn "no location found for element %s" elementId
        OnMouseDown (fun (ev : Browser.Types.MouseEvent) -> ())

    let defaultMouseMoveListener dispatch =
      OnMouseMove (fun (ev : MouseEvent) ->
        ev.preventDefault()
        let c = coords ev.clientX ev.clientY
        OnDrag c |> dispatch
      )

    /// Listener for when another element is being dragged and is moved over this element.
    let defaultHoverListener model id dispatch =
      OnMouseEnter (fun (ev : MouseEvent) ->
        ev.preventDefault()
        let loc = getLocationForElement id model
        match loc with
        | Some loc ->
          DragOver loc |> dispatch
        | None -> ()
      )

    let defaultReleaseListener dispatch =
      OnMouseUp (fun (ev : MouseEvent) -> ev.preventDefault();  DragEnd |> dispatch)

  type DropAreaProp =
  | AreaStyle of CSSProp list
  | AreaProps of IHTMLProp list

  module PropertyFolding =

    type FoldState = CSSProp list * IHTMLProp list
    let foldStateZero = ([], [])

  //   let foldWithMatcher (props: DragProp list) matcher : IHTMLProp list =
  //     let styles, htmlProps =
  //       props
  //       |> List.fold(fun (styles, htmlProps) x -> matcher (styles, htmlProps) x 
  //       ) foldStateZero
  //     [
  //       yield Style styles
  //       yield! htmlProps
  //     ]

  //   // let foldDraggable (dragProps: DragProp list) : IHTMLProp list =
  //   //   let matcher (styles, properties) x =
  //   //     match x with
  //   //     | DraggableStyle s -> (s @ styles, properties)
  //   //     | DraggableProps p -> (styles, p @ properties)
  //   //     | _ -> (styles, properties)
  //   //   foldWithMatcher dragProps matcher

  //   // let foldDragged (dragProps : DragProp list) : IHTMLProp list =
  //   //   let matcher (styles, props) x =
  //   //     match x with
  //   //     | DraggedStyle s -> (s @ styles, props)
  //   //     | DraggedProps p -> (styles, p @ props)
  //   //     | _ -> (styles, props)
  //   //   let added = [ PointerEvents "None" ] |> DraggedStyle
  //   //   foldWithMatcher (added :: dragProps) matcher

  //   // let foldPreview (dragProps : DragProp list) : IHTMLProp list =
  //   //   let matcher (styles, props) x =
  //   //     match x with
  //   //     | PreviewStyle s -> (s @ styles, props)
  //   //     | PreviewProps p -> (styles, p @ props)
  //   //     | _ -> (styles, props)
  //   //   foldWithMatcher dragProps matcher


  //   // // called when something is being dragged but it's not this element
  //   // let foldDraggableDuringDrag (dragProps : DragProp list) : IHTMLProp list =
  //   //   let matcher (styles, properties) x =
  //   //     match x with
  //   //     | DraggableStyle s -> (s @ styles, properties)
  //   //     | DraggableProps p -> (styles, p @ properties)
  //   //     | _ -> (styles, properties)
  //   //   foldWithMatcher dragProps matcher

    let foldDropArea props : IHTMLProp list =
      let styles, htmlProps =
        props
        |> List.fold(fun (styles, htmlProps) x ->
          match x with
          | AreaStyle s -> (s @ styles, htmlProps)
          | AreaProps p -> (styles, p @ htmlProps)
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
          //| _ -> (styles, htmlProps)
        ) foldStateZero
      [
        yield Style styles
        yield! htmlProps
      ]

  open PropertyFolding

  // type ElementProps =
  // | Styling of CSSProp list
  // | Properties of IHTMLProp list

  // let collectElementProps (eleProps : ElementProps list) =
  //   let f (styles, props) x =
  //     match x with
  //     | Styling s -> (s @ styles, props)
  //     | Properties p -> (styles, p @ props)
  //   eleProps |> List.fold (fun (s, p) x -> f (s, p) x) ([], [])

  // type DraggableProp =
  // /// Styling for the element being dragged
  // | DraggedElement of ElementProps list
  // /// Styling for a preview item to show where an item will be placed when dropped
  // | HoverPreviewElement of ElementProps list
  // /// Items that are able to be dragged. Most items will be this most of the time. Think of it like "default".
  // | DraggableElement of ElementProps list
  // /// Styling for an element sliding out of the way to accomodate a drop.
  // | SlidingElement of ElementProps list
  // /// ALL elements will have this class applied; if you want to override it, you can do so with properties specific to that type of element.
  // | DefaultClass of className : string

  type DraggableTemplate = {
    DraggedElementStyles : CSSProp list option
    DraggedElementProperties : IHTMLProp list option
    HoverPreviewElementStyles : CSSProp list option
    HoverPreviewElementProperties : IHTMLProp list option
    SlidingElementStyles : CSSProp list option
    SlidingElementProperties : IHTMLProp list option
    DraggableElementStyles : CSSProp list option
    DraggableElementProperties : IHTMLProp list option
    DefaultClass : string option
  } with
    static member Empty() = {
      DraggedElementStyles = None
      DraggedElementProperties = None
      HoverPreviewElementStyles = None
      HoverPreviewElementProperties = None
      SlidingElementStyles = None
      SlidingElementProperties = None
      DraggableElementStyles = None
      DraggableElementProperties = None
      DefaultClass = None
    }

  type Messaging = {
    Model : Model
    Id : ElementId
    Dispatch : (Msg -> unit)
  } with
    static member Create mdl id dis = {
      Model = mdl
      Id = id
      Dispatch = dis
    }

  // let getDraggableElement (elements : DraggableProp list) =
  //   elements
  //   |> List.tryPick (fun x ->
  //     match x with
  //     | DraggableElement eleProps -> Some eleProps
  //     | _ -> None
  //   )
  //   |> Option.map collectElementProps
  //   |> Option.defaultValue ([], [])

  // let getDraggedElement (elements : DraggableProp list) =
  //   elements
  //   |> List.tryPick (fun x ->
  //     match x with
  //     | DraggedElement eleProps -> Some eleProps
  //     | _ -> None
  //   )
  //   |> Option.map collectElementProps
  //   |> Option.defaultValue ([], [])

  // let getPreviewElement (elements : DraggableProp list) =
  //   elements
  //   |> List.tryPick (fun x ->
  //     match x with
  //     | HoverPreviewElement eleProps -> Some eleProps
  //     | _ -> None
  //   )
  //   |> Option.map collectElementProps
  //   |> Option.defaultValue ([], [])

  // let getSlidingElement (elements : DraggableProp list) =
  //   elements
  //   |> List.tryPick (fun x ->
  //     match x with
  //     | SlidingElement eleProps -> Some eleProps
  //     | _ -> None
  //   )
  //   |> Option.map collectElementProps
  //   |> Option.defaultValue ([], [])

  // let getDefaultClass (elements : DraggableProp list) =
  //   elements
  //   |> List.tryPick (fun x ->
  //     match x with
  //     | DraggableProp.DefaultClass defaultClass -> Some defaultClass
  //     | _ -> None
  //   )
  //   |> Option.map (fun x -> (ClassName x ) :> IHTMLProp)

  let private foldStylesAndProps styles props =
    [
      (Style styles) :> IHTMLProp
      yield! props
    ]

  // let private appendProperty prop (styles, props) = (styles, prop :: props)
  // let private maybeAppendProperty mprop (styles, props) =
  //   match mprop with
  //   | Some prop -> styles, prop :: props
  //   | None -> styles, props
  // let private appendStyle style (styles, props) = (style :: styles, props)

  module private Rendering =
    let private defaultList lo = Option.defaultValue [] lo

    let defaultClassList defaultClass = 
      match defaultClass with
      | Some _class -> [ (ClassName _class) :> IHTMLProp ]
      | None -> []

    let renderDraggable msging defaultClass styles props content =
      let idProp = (Id msging.Id) :> IHTMLProp
      let listener = (Listeners.defaultDraggable msging.Model msging.Id msging.Dispatch) :> IHTMLProp
      let styles = defaultList styles
      match defaultClass with
      | Some _class ->
        let classProp = (ClassName _class) :> IHTMLProp
        let props = (defaultList props) @ [ listener ; idProp; classProp ] 
        let htmlProps = foldStylesAndProps styles props
        div htmlProps content
      | None ->
        let props = (defaultList props) @ [ listener ; idProp ] 
        let htmlProps = foldStylesAndProps styles props
        div htmlProps content

    let renderDraggableWithHoverListener msging defaultClass styles props content =
      let idProp = (Id msging.Id) :> IHTMLProp
      let styles = defaultList styles
      let listener = (Listeners.defaultHoverListener msging.Model msging.Id msging.Dispatch) :> IHTMLProp
      match defaultClass with
      | Some _class ->
        let classProp = (ClassName _class) :> IHTMLProp
        let props = (defaultList props) @ [ listener ; idProp; classProp ] 
        let htmlProps = foldStylesAndProps styles props
        div htmlProps content
      | None ->
        let props = (defaultList props) @ [ listener ; idProp ] 
        let htmlProps = foldStylesAndProps styles props
        div htmlProps content

    let renderDragged msging defaultClass styles props content =
      let idProp = (Id msging.Id) :> IHTMLProp
      let appendedStyles = [
        PointerEvents "none"
        Left msging.Model.Cursor.x
        Top msging.Model.Cursor.y
      ]
      let styles = (defaultList styles) @ appendedStyles
      match defaultClass with
      | Some _class ->
        let classProp = (ClassName _class) :> IHTMLProp
        let props = (defaultList props) @ [ idProp; classProp ]
        let htmlProps = foldStylesAndProps styles props
        div htmlProps content
      | None ->
        let props = (defaultList props) @ [ idProp ]
        let htmlProps = foldStylesAndProps styles props
        div htmlProps content

    let renderHoverPreview msging defaultClass styles props content =
      let _class = defaultClassList defaultClass
      let idProp = (Id msging.Id) :> IHTMLProp
      match styles, props with
      | Some styles, Some props ->
        let props = props @ (idProp ::_class )
        let htmlProps = foldStylesAndProps styles props
        div htmlProps content
      | Some styles, None ->
        let props =  idProp ::_class 
        let htmlProps = foldStylesAndProps styles props
        div htmlProps content
      | None, Some props ->
        let props = props @ (idProp ::_class )
        let htmlProps = foldStylesAndProps [] props
        div htmlProps content
      | _ -> Html.none //no styles or props given, don't render a preview

    /// this is the "invisible" placeholder item we render so the slider can calculate where to slide to
    let private renderSlideGhost msging defaultClass styles props content =
      printfn "rendering slide ghost"
      let _class = defaultClassList defaultClass
      let idProp = (Id (msging.Id + "-ghost")) :> IHTMLProp
      /// if the user mouses back over the ghost, the elements should move back
      let listener = (Listeners.defaultHoverListener msging.Model msging.Id msging.Dispatch) :> IHTMLProp
      let additionalStyles = [
        Opacity 0.001
        Position PositionOptions.Absolute
      ]
      let styles = styles @ additionalStyles
      let props = props @ (idProp :: (listener :: _class))
      let htmlProps = foldStylesAndProps styles props
      div htmlProps content

    /// this is the actual sliding content
    let private renderSlider slide msging defaultClass styles props content =
      let _class = defaultClassList defaultClass
      let target = Browser.Dom.document.getElementById(msging.Id + "-ghost")
      let idProp = (Id (msging.Id + "-slider")) :> IHTMLProp
      if isNull target then
        //we don't yet have our slide target; render the slider but don't give it any movement
        let left = CSSProp.Left slide.StartCoords.x
        let top = Top slide.StartCoords.y
        let styles = styles @ [ left; top ]
        let props = props @ (idProp :: _class)
        let htmlProps = foldStylesAndProps styles props
        div htmlProps content
      else
        // if the slider is not null, then we have rendered a sliding object.
        // we must re-render (that's how this render loop works) but we should redraw it
        // where it currently is and make it keep sliding
        let rect = target.getBoundingClientRect()
        let left = CSSProp.Left rect.left
        let top = Top rect.top
        let styles = styles @ [ left; top ]
        let props = props @ [ idProp ]
        let htmlProps = foldStylesAndProps styles props
        div htmlProps content

    let renderSlidingElement slide msging defaultClass styles props content =
      printfn "Rendering slide element: %A" slide
      let slider = renderSlider slide msging defaultClass [] props content
      let ghost = renderSlideGhost msging defaultClass [] props content
      div [] [ slider; ghost ]

  let determineChildRender template msging content =
    match msging.Model.Moving with
    | None ->
      // no moving; render all content as draggable
      Rendering.renderDraggable msging template.DefaultClass template.DraggableElementStyles template.DraggableElementProperties content
    | Some { StartLocation = (startList, startIndex, elementId); Slide = None } ->
      // there is a drag happening, but no elements are sliding
      if msging.Id = elementId then
        //this is the element being dragged; render it and the hover preview element
        let draggedElement = Rendering.renderDragged msging template.DefaultClass template.DraggedElementStyles template.DraggedElementProperties content
        let hoverPreviewElement = Rendering.renderHoverPreview msging template.DefaultClass template.HoverPreviewElementStyles template.HoverPreviewElementProperties content
        div [] [
          draggedElement
          hoverPreviewElement
        ]
      else
        // render normal content with listener for mouse hovering over this spot
        Rendering.renderDraggableWithHoverListener msging template.DefaultClass template.DraggableElementStyles template.DraggableElementProperties content
    | Some { StartLocation = (startList, startIndex, elementId); Slide = Some slide } ->
      // there is a drag happening, and an element is sliding
      if msging.Id = elementId then
        //this is the element being dragged; render it and the hover preview element
        let draggedElement = Rendering.renderDragged msging template.DefaultClass template.DraggedElementStyles template.DraggedElementProperties content
        let hoverPreviewElement = Rendering.renderHoverPreview msging template.DefaultClass template.HoverPreviewElementStyles template.HoverPreviewElementProperties content
        div [] [
          draggedElement
          hoverPreviewElement
        ]
      elif msging.Id = slide.ElementId then
        //this is the sliding element; render it, render the ghost, and then cry cause it's broken
        //Html.none
        match template.SlidingElementStyles, template.SlidingElementProperties with
        | None, None ->
          //no styles or props given, don't render a slider, just the item itself
          Rendering.renderDraggableWithHoverListener msging template.DefaultClass template.DraggableElementStyles template.DraggableElementProperties content
        | Some styles, Some props ->
          Rendering.renderSlidingElement slide msging template.DefaultClass styles props content
        | Some styles, None ->
          Rendering.renderSlidingElement slide msging template.DefaultClass styles [] content
        | None, Some props ->
          Rendering.renderSlidingElement slide msging template.DefaultClass [] props content
      else
        // render normal content with listener for mouse hovering over this spot
        Rendering.renderDraggableWithHoverListener msging template.DefaultClass template.DraggableElementStyles template.DraggableElementProperties content


  type DropArea =

    static member dropArea model dispatch (props : DropAreaProp list) template msgContentList =
      match model.Moving with
      | None ->
        let htmlProps = foldDropAreaWithoutListeners props
        let children = 
          msgContentList
          |> List.map(fun (msging, content) ->
            determineChildRender template msging content 
          )
        div htmlProps children
      | Some _ ->
        let htmlProps =
          [
            yield! foldDropArea props
            (Listeners.defaultReleaseListener dispatch) :> IHTMLProp
            (Listeners.defaultMouseMoveListener dispatch) :> IHTMLProp
          ]
        let children = 
          msgContentList
          |> List.map(fun (msging, content) ->
            determineChildRender template msging content 
          )
        div htmlProps children

  let private split i li =
    let len x = List.length x
    let first = if len li >= i then List.take i li else li
    let second = if len li <= i then [] else List.skip i li
    first, second

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

  let moveItem (startListIndex, startIndex) (insertListIndex, insertAtIndex) li =
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

  let tryGetSlide elementId =
    match tryGetElementCoords elementId with
    | Some coords -> Slide.Create coords elementId |> Some
    | None -> None

  let update msg model =
    match msg with
    | DragStart (loc, startCoords, offset) ->
      let movingStatus = MovingStatus.Init (loc) |> Some
      { model with Moving = movingStatus; Cursor = startCoords; Offset = Some offset }, Cmd.none
    | OnDrag coords ->
      {model with Cursor = coords }, Cmd.none
    | DragOver (listIndex, index, elementId) ->
      printfn "Drag over %A" (listIndex, index, elementId)
      match model.Moving with
      | Some { StartLocation = (startList, startIndex, startingElementId) }->
        let slide = tryGetSlide elementId
        let items' =
          moveItem (startList, startIndex) (listIndex, index) model.Items
          |> ModelFuncs.updateItemLocations
        let mdl = { model with Items = items' } |> ModelFuncs.buildItemDict |> ModelFuncs.setSlideOpt slide
        let newStartLoc = ModelFuncs.getItemLocation startingElementId mdl
        match newStartLoc with
        | None ->
          mdl, Cmd.none
        | Some loc ->
          let mdl = ModelFuncs.setDragSource loc mdl
          mdl, Cmd.none
      | None ->
        model, Cmd.none
    // | SlideStart slidingElementId ->
    //   match model.Moving with
      // | Some moving ->
      //   model, Cmd.none
      // | None ->
      //   model, Cmd.none
    | DragEnd ->
      { model with Moving = None; Offset = None }, Cmd.none