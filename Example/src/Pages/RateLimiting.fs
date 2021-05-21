module RateLimiting

/// follow the mouse cursor and place an 'x'
/// every 20 ms

open Elmish
open System

type Position = {
  X : float
  Y : float
}
let pos x y = {
  X = x
  Y = y
}

type Msg =
| MouseMove of Position
| Mark of Position
| LimiterMsg of RateLimiter.Msg<Msg>
| MouseMoveWithThrottle of Position * ThrottleEvents.Msg
| ThrottleMsg of ThrottleEvents.Msg

type Model = {
  Marks : Position list
  LimiterState : RateLimiter.State<Msg>
  ThrottlerState : ThrottleEvents.Throttler
}

let timespan = TimeSpan.FromMilliseconds(500.)

let update msg model =
  match msg with
  | MouseMove pos ->
    printfn "mouse moved to %A" pos
    let limiterMdl, limiterCmd = RateLimiter.limitLossy (timespan) "marks" (Mark pos) model.LimiterState

    { model with LimiterState = limiterMdl }, Cmd.map LimiterMsg limiterCmd
  | LimiterMsg limiterMsg ->
    let limiterMdl, externalCmd = RateLimiter.update limiterMsg model.LimiterState
    { model with LimiterState = limiterMdl }, externalCmd
  | Mark pos ->
    printfn "adding mark %A" pos
    { model with Marks = pos :: model.Marks }, Cmd.none
  | MouseMoveWithThrottle (pos, throttleMsg) ->
    printfn "Handling mouse move at pos %A with throttle" pos
    let mdl = { model with Marks = pos :: model.Marks }
    // send the throttle message to be handled
    mdl, Cmd.ofMsg (ThrottleMsg throttleMsg)
  | ThrottleMsg (throttleMsg) ->
    printfn "handling throttle message of %A" throttleMsg
    match ThrottleEvents.handleThrottleMsg2 throttleMsg model.ThrottlerState with
    | Ok (ts, cmd) ->
      { model with ThrottlerState = ts }, Cmd.map ThrottleMsg cmd
    | Error e ->
      printfn "Error in throttler: %s" e
      model, Cmd.none

let init() = {
  Marks = []
  LimiterState = RateLimiter.createState()
  ThrottlerState = ThrottleEvents.init()
}

open Fable.React
open Fable.React.Props
open Fable.React.DrawingCanvas
open Fable.React.DrawingCanvas.Builder

let canvas model = 
  let elements = model.Marks
  let len = List.length elements
  if len > 0 then
    drawing {
      repeat [ 0 .. (len - 1)](fun i ->
        let element = elements.[i]
        preserve {
          font "20px Georgia"
          fillStyle (DrawStyle.Color "black")
          fillText "x" element.X element.Y 0.
        }
      )
    }
  else
    drawing {
      font "20px Georgia"
      fillStyle (DrawStyle.Color "black")
      fillText "x" 0. 0. 0.
    }

let drawElements model =
  model.Marks
  |> List.map (fun m ->
    p [ Style [ CSSProp.Position PositionOptions.Fixed; CSSProp.Left m.X; CSSProp.Top m.Y ]] [ str "x"]
  )

let ts = TimeSpan.FromMilliseconds(20.)

let view model (dispatch : Msg -> unit) =
  div [
    OnMouseMove (fun ev -> 
      match ThrottleEvents.throttle model.ThrottlerState "mouse-move-ev" ts ev with
      | None -> ()
      | Some (ev, throttleMsg) ->
        let mousePos = pos ev.clientX ev.clientY
        let throttleMsg = throttleMsg
        MouseMoveWithThrottle (mousePos, throttleMsg) |> dispatch
    )

  ] [
    yield! drawElements model
    p [ Style [ CSSProp.TextAlign TextAlignOptions.Center; CSSProp.Height 1000; CSSProp.Width 1000 ]] [ str "move mouse to draw a trail" ]
  ]


