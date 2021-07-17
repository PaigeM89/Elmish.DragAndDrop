namespace Elmish.DragAndDropHelpers

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
    elif len li = 0 then
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

  let split i li =
    let len x = List.length x
    let first = if len li >= i then List.take i li else li
    let second = if len li <= i then [] else List.skip i li
    first, second

module HelperTypes =
  type ElementId = string
  type DraggableId = string

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

  type DropAreaId = string
  type CurrentDropArea = ListIndex * DropAreaId option

  type MovingStatus = {
      Slide : Slide option
      StartLocation : ItemLocation
      CurrentDropArea : CurrentDropArea option
    } with
        member this.SetSlide (slide : Slide option) = { this with Slide = slide }
        static member Init loc =
          { Slide = None; StartLocation = loc; CurrentDropArea = Some (locListIndex loc, None) }

module BrowserHelpers =
  open HelperTypes

  let getDraggedElement id =
      let doc = Browser.Dom.document
      let ele = doc.getElementById(id)
      ele

  let getLeftTopForElement id =
    let ele = getDraggedElement id
    let rect = ele.getBoundingClientRect()
    let x = rect.left
    let y = rect.top
    x, y
  
  let getOffset ev id =
    let ele = getDraggedElement id
    let rect = ele.getBoundingClientRect()
    let coords = fromME ev
    let x = coords.x - rect.left
    let y = coords.y - rect.top
    { x = x; y = y}