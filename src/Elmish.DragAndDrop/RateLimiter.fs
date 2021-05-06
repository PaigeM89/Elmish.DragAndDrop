namespace Elmish

module RateLimiter =

  open System
  open Fable.Core
  open Elmish

  type Id = string

  type Pending<'a> =
  | Lossy
  | Single of msg : 'a
  //| Lossless of msgs : System.Collections.Generic.Queue<'a>


  type State<'a> = {
    /// Map of some Id to track this event, and the next message to send
    Limited: Map<Id,Pending<'a>>
  }

  let createState() = {
    State.Limited = Map.empty
  }

  type Msg<'a> =
  //| Delay
  | Free of Id // * a : 'a //* 'AppMsg
  | ExternalMsg of msg : 'a
  | OnError of exn

  // free the id in X time
  let freeCmd (id : Id) ms =
    printfn "creating promise to free %s in %i" id ms
    promise {
      do! Promise.sleep ms
      return id
    }

  /// Limits events to once per a specified time span per source. If an event is given while that element is 
  /// under a rate limit, that event is lost and will never be invoked.
  let limitLossy<'a> (onePer : TimeSpan) (id : Id) (msgToSend: 'a) (currentState : State<'a>) =
    /// if we have no current delay, invoke this event and register this id as delayed for X time
    /// if we have a delay, ignore the event
    match Map.tryFind id currentState.Limited with
    | Some (_) -> // we have state for this id, ignore event and continue
      currentState, Cmd.none
    | None ->
      let m = Map.add id Lossy currentState.Limited
      let timer = onePer.TotalMilliseconds |> int
      { currentState with Limited = m }, Cmd.batch [
        Cmd.ofMsg (ExternalMsg msgToSend)
        Cmd.OfPromise.either (freeCmd (id)) (timer) Free OnError
      ]


  /// Limits events to one per a specified time span per source. If an event is given while that element is
  /// under a rate limit, the event is stored. If no events replace it, then that event is fired when the time is up.
  /// Events are lost if they are replaced, but the last event is always fired.
  let limitToLatest (onePer : TimeSpan) (id : Id) (msgToSend: 'a) (currentState : State<'a>) =
    // if we have no current delay, invoke this event and register this id as delayed for X time
    // if we have a delay, set this event as the one to be invoked when the timer is up
    let ms = onePer.TotalMilliseconds |> int
    match Map.tryFind id currentState.Limited with
    | Some (_) -> // we have state for this id, ignore event and continue
      let m = Map.add id (Single msgToSend) currentState.Limited
      // we assume there is still a valid timer for this Id that will fire in the future.
      { currentState with Limited = m }, Cmd.none
        //Cmd.OfPromise.either (freeCmd id) ms Free OnError
    | None ->
      // init with Lossy; when this is called again, Lossy will be replaced with the queued message.
      let m = Map.add id Lossy currentState.Limited
      { currentState with Limited = m }, Cmd.batch [
        Cmd.ofMsg (ExternalMsg msgToSend)
        Cmd.OfPromise.either (freeCmd id) (ms) Free OnError
      ]

  /// Limits events to one per a specified time span per source. If an event is given while that element is
  /// under a rate limit, the event is stored. All events are added to a queue, and are popped off the queue one by one as the time span elapses.
  /// No events are lost (unless the page is left or some other fundamental state change is made).
  // let limitLossless (onePer : TimeSpan) (id : Id) (msgToSend: 'a) (currentState : State<'a>) =
  //   ()

  let update (msg : Msg<'a>) (currentState : State<'a>) : (State<'a> * Cmd<'a>) =
    match msg with
    | ExternalMsg eMsg ->
      printfn "raising external msg: %A" eMsg
      currentState, Cmd.ofMsg eMsg
    | Free id -> 
      match Map.tryFind id currentState.Limited with
      | Some (Lossy) ->
        let m = Map.remove id currentState.Limited
        { currentState with Limited = m}, Cmd.none
      | Some (Single msgToSend) ->
        let m = Map.remove id currentState.Limited
        { currentState with Limited = m}, Cmd.OfFunc.result msgToSend
      | None ->
        currentState, Cmd.none
    | OnError e ->
      JS.console.warn (sprintf "Rate Limiter error: %A" e)
      currentState, Cmd.none

module ThrottleEvents =
  open System
  open Browser.Types
  open Elmish

  /// Usage:
  /// When handling an event (like a mouse move event), call `throttle`, passing in
  /// the state (which is `Throttler`), the id of the event being throttled (such as `mouse-event`; all 
  /// events being throttled from the same source should have the same name), the time to throttle, and the event itself.
  /// If that id is free, then the event is returned, and a message to throttle is also returned
  ///     That message must be  handled by the caller, where it is passed down to this throttler and mapped 
  ///     to a new state & message.

  type Status = 
  | Throttled
  | Free
  type Id = string
  type Throttler = Map<Id, Status>
  let init() = Map.empty
  
  type Msg =
  | Throttle of Id * TimeSpan
  | Release of Id
  | OnError of exn

  // free the id in X time
  let freePromise (id : Id) ms =
    printfn "creating promise to free %s in %i" id ms
    promise {
      do! Promise.sleep ms
      return id
    }

  let getReleaseCmd (msg : Msg) mdl =
    match msg with
    | Throttle (id, ts) ->
      let ms = (ts.TotalMilliseconds |> int)
      let promise = freePromise id
      let mdl = Map.add id Throttled mdl
      mdl, Cmd.OfPromise.either promise (ms) Release OnError
    | _ -> mdl, Cmd.none

  let throttle (throttler : Throttler) id (ts : TimeSpan) (ev) =
    match Map.tryFind id throttler with
    | Some Status.Throttled -> None
    | Some Status.Free
    | None ->
      let msg = Throttle (id, ts)
      Some (ev, msg)

  let handleThrottleMsg msg mdl =
    match msg with
    | Release id -> Map.remove id mdl |> Ok
    | Throttle (id, _) -> Map.add id Throttled mdl |> Ok
    | OnError exn -> sprintf "Throttler error: %A" exn |> Error

  let handleThrottleMsg2 msg mdl =
    match msg with
    | Release id -> (Map.remove id mdl, Cmd.none) |> Ok
    | Throttle (id, ts) ->
      let ms = (ts.TotalMilliseconds |> int)
      let promise = freePromise id
      let mdl = Map.add id Throttled mdl
      (mdl, Cmd.OfPromise.either promise (ms) Release OnError) |> Ok
    | OnError exn -> 
      sprintf "Throttler error: %A" exn |> Error