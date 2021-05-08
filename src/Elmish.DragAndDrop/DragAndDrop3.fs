namespace Pages

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

  /// Create an empty Model instance
  let init() = {
    Cursor = { x = 0. ; y = 0.}
    Moving = None, None
    Offset = None
    Items = []
  }

  let getLocationForElement elementId model = 
    model.Items
    |> List.map (fun li -> li |> List.tryFind (fun ((_, _, id), _) -> id = elementId))
    |> List.choose id
    |> List.tryHead

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

  // [<AutoOpen;EditorBrowsable(EditorBrowsableState.Never);Erase>]
  // module Types =
  //   [<EditorBrowsable(EditorBrowsableState.Never)>]
  //   type HandleProps =
  //     abstract styles : CSSProp list
  //     abstract classname : string
  //     abstract children : ReactElement
    
  //   type IDragAndDropProperty = interface end

  // /// A specific element that is designed to be dragged, as part of a larger item that will move with it.
  // [<Erase>]
  // type Handle =
  //   static member inline classname (classname : string) = unbox<IDragAndDropProperty>("class", classname)
  //   static member inline styles (props : CSSProp list) = unbox<IDragAndDropProperty>("style", props)
  //   static member inline position (pos : PositionOptions) = Position pos

  // let Handle props children = 
  //   div [
  //     props
  //   ] children
  
  module Styles =
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

    /// `IHtmlProp` properties for an item that is able to be dragged
    let IsDraggable loc dispatch : IHTMLProp list= [
      Style [
        CSSProp.Custom ("draggable", true)
        CSSProp.Cursor "grab"
      ]
      DOMAttr.OnMouseDown(fun ev ->
        ev.preventDefault()
        let o = Helpers.getOffset ev (locId loc)
        (loc, fromME ev, o) |> DragStart |> dispatch
      )
    ]

    /// `IHTMLProp` properties for an item that is a preview of where a drop will go
    let IsPreview() : IHTMLProp list = [
      Style [
        Opacity 0.2f
        PointerEvents "none"
      ]
      Id "preview"
    ]

    /// Mouse Enter listener to know when an item may have to move or shift to accomdate a drop.
    let IsListener loc dispatch : IHTMLProp list = [
      DOMAttr.OnMouseEnter(fun ev ->
        ev.preventDefault()
        DragOver(loc) |> dispatch
      )
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

  /// An element that is designed to be dragged
  type Draggable =
    static member inline handle (ele : ReactElement) = ()
    
    static member inline draggable model elementid dispatch props children  = 
      let loc = getLocationForElement elementid model
      div [
        yield! props
        match loc with
        | Some (loc, _) ->
          DOMAttr.OnMouseDown(fun ev ->
            ev.preventDefault()
            let o = Helpers.getOffset ev (locId loc)
            (loc, fromME ev, o) |> DragStart |> dispatch
          )
        | None -> ()
      ] children

    /// An element that is dragged by a smaller element within it, but the whole element is moved.
    static member inline draggableWithHandle model elementid dispatch handleProps handleChildren props children = 
      let loc = getLocationForElement elementid model
      div [
        yield! props
      ] [
        yield div [
          yield! handleProps
          match loc with
          | Some (loc, _) ->
            DOMAttr.OnMouseDown(fun ev ->
              ev.preventDefault()
              let o = Helpers.getOffset ev (locId loc)
              (loc, fromME ev, o) |> DragStart |> dispatch
            )
          | None -> ()
        ] handleChildren
        yield! children
      ]

  type DropArea =
    static member inline SortedDropArea model index dispatch props =
      match model.Moving with
      | Some(movingIndex, elementId), Some slideState ->
        // an item is being dragged and another element is sliding
        model.Items.[index] |> List.map(fun (loc, content) ->
          let id = locId loc
          if id = elementId then
            let coords = model.Cursor -- model.Offset
            // todo: we may want to drag the element's properties along so we can avoid an extra div here
            // since any styling on the content will need to be added to it directly and wrapped here
            let hover = div [ yield! Styles.IsDragged coords ] [content]
            let preview = div [ yield! Styles.IsPreview() ] [ content ]
            div [] [ hover; preview ]
          elif slideState.ElementId = elementId then
            let slider = div [ yield! Styles.IsSliding slideState ] [ content ]
            let element = div [ Style [ Opacity 0.01 ] ] [ content ]
            div [] [
              slider
              element
            ]
          else
            div [ yield! Styles.IsListener loc dispatch ] [ content ]
        )
      | Some(movingIndex, elementId), None ->
        // an element is being dragged but nothing is sliding
         model.Items.[index] |> List.map(fun (loc, content) ->
          let id = locId loc
          if id = elementId then
            let coords = model.Cursor -- model.Offset
            // todo: we may want to drag the element's properties along so we can avoid an extra div here
            // since any styling on the content will need to be added to it directly and wrapped here
            let hover = div [ yield! Styles.IsDragged coords ] [content]
            let preview = div [ yield! Styles.IsPreview() ] [ content ]
            div [] [ hover; preview ]
          else
            div [ yield! Styles.IsListener loc dispatch ] [ content ]
        )
      | None, _ ->
        model.Items.[index] |> List.map(fun (loc, content) ->
          div [ yield! Styles.IsDraggable loc dispatch ] [ content ]
        )
