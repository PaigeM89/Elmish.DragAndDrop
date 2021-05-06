namespace Elmish

module DragAndDrop2 =

  open System
  open Elmish
  open Feliz
  open Fable.React
  open Fable.React.Props

  type ElementId = string

  type Coords = { x : float; y : float}

  let coords x y = { x = x; y = y }

  type Rect = { x : float; y : float; wedight : float; height: float }

  type Model = {
    Cursor : Coords
    Moving : ElementId option
    OnDrag : (ElementId -> unit) option
    OnDragEnter: (ElementId -> unit) option
  }

  let init() = {
    Cursor = { x = 0. ; y = 0.}
    Moving = None
    OnDrag = None
    OnDragEnter = None
  }

  type Msg =
  | DragStart of elementId : ElementId
  | OnDrag of elementId : ElementId * coords : Coords
  | DragEnter of target : ElementId
  | DragLeave
  | DragOver
  | DragEnd


  let DraggableType (t : string) = CSSProp.Custom ("draggableType", t)
  let IsDropArea (t : string) = CSSProp.Custom ("drop-bucket", t)

  let IsDraggable id (t : string) dispatch : IHTMLProp list= [
    Style [
      CSSProp.Custom ("draggable", true)
      DraggableType t
      CSSProp.Cursor "grab"
    ]
    DOMAttr.OnMouseDown(fun ev ->
      ev.preventDefault()
      id |> DragStart |> dispatch
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
      //let c = coords ev.pageY ev.pageY
      let c = coords ev.clientX ev.clientY
      OnDrag (id, c) |> dispatch
    )
    OnMouseUp (fun ev -> ev.preventDefault();  DragEnd |> dispatch)
  ]

  let draggable model dispatch id _class content =
    match model.Moving with
    | Some elementId when elementId = id ->
      div [ yield! IsDragged id "test" model.Cursor dispatch; ClassName (_class + " dragged"); Id id ] [ content ]
    | Some _ ->
      // if we have an active drag, dont render anything as draggable
      div [ ClassName _class; Id id ] [ content ]
    | None ->
      div [ yield! IsDraggable id "test" dispatch; ClassName _class; Id id ] [ content ]


  module Components =
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props

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
            //this is required
            //let c = coords ev.clientX ev.clientY
            //OnDrag (id, c) |> dispatch
          )
          OnMouseUp (fun ev -> ev.preventDefault();  DragEnd |> dispatch)
      ] contents

  let dropArea model dispatch = 
    let left = 
      Components.DropArea("left-container", "test", "container left", model.Moving.IsSome, dispatch, 
        [
            draggable model dispatch "draggable-1" "content" (p [] [ str "This is some content" ])
            div [] [
                p [] [ str "And some content with an input box" ]
                input [ ]
            ] |> draggable model dispatch "draggable-2" "content"
            p [ ClassName "content" ] [ str "And yet even more" ] 
        ]
      )

    let right =
        div [Id "right-container"; ClassName "container right" ] [
            p [ ClassName "content dad-unselectable" ] [ str "This is is also content" ]
            p [ ClassName "content dad-unselectable" ] [ str "And some more content" ]
            h3 [ ClassName "content dad-unselectable" ] [ str "And more content, but different" ]
        ]

    div [ ] [
        left
    ]


  let update msg model =
    match msg with
    | DragStart element ->
      printfn "Drag start for element %A" element
      { model with Moving = Some element }, Cmd.none
    | OnDrag (element, coords) ->
      printfn "onDrag for element %A at coords %A" element coords
      match model.OnDrag with
      | Some f -> f element
      | None -> ()
      {model with Cursor = coords }, Cmd.none
    | DragEnter element ->
      match model.OnDragEnter with
      | Some f -> f element
      | None -> ()
      model, Cmd.none
    | DragEnd ->
      printfn "Drag end for element"
      { model with Moving = None }, Cmd.none
    | _ -> model, Cmd.none
