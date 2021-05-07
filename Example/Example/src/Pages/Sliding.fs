namespace Pages


module Sliding =
  module React = 
    open System
    open Fable.React
    open Fable.React.Props
    open Feliz

    [<ReactComponent>]
    let AnimationsOnHover(content : ReactElement list) =
      let (hovered, setHovered) = React.useState(false)
      Html.div [
          prop.style [
              style.padding 10
              style.transitionDuration (TimeSpan.FromMilliseconds 1000.0)
              style.transitionProperty [
                  transitionProperty.backgroundColor
                  transitionProperty.color
              ]

              if hovered then
                 style.backgroundColor.lightBlue
                 style.color.black
              else
                 style.backgroundColor.limeGreen
                 style.color.white
          ]
          prop.onMouseEnter (fun _ -> setHovered(true))
          prop.onMouseLeave (fun _ -> setHovered(false))
          prop.children content
      ]

    [<ReactComponent>]
    let Slide(content : ReactElement list) = 
      let isMoving, setIsMoving = React.useState(false)
      Html.div [
        prop.style [
          style.position.absolute
          style.padding 20
          if isMoving then style.left 1000 else style.left 500
          style.top 500
          style.transitionDurationMilliseconds 1000.0
          style.transitionProperty "left"
        ]
        prop.children [
          button [ OnClick (fun _ -> setIsMoving(not isMoving))] [ str "move" ]
          yield! content
        ]
      ]

    type Model = {
      ShouldSlide : bool
    } with
      static member Init() = { ShouldSlide = false }

    type Msg =
    | Loading
    | Hoist
    | Loaded
    | LoadError of exn

    type Props = { initCount : int }
    type State = { count : int }

    // type Counter2(props) =
    //   inherit Component<Props, State>(props)
    //   do base.setInitState({ count = props.initCount })
    //   override this.render() =
    //     let b = 
    //       button [
    //         OnClick (fun _ -> this.setState(fun s p -> { s with count = s.count + 1 }))
    //       ] [ str (sprintf "counter : %i" this.state.count)]
    //     b :> ReactElement

    let Counter3 = 
      FunctionComponent.Of(fun (props: {| initCount: int |}) ->
        let state = Hooks.useState(props.initCount) // This is where the magic happens
        button
            [ OnClick (fun _ -> state.update(fun s -> s + 1)) ]
            [ str "Times clicked: "; ofInt state.current ]
        )

    let Animations =
      FunctionComponent.Of(fun () ->
        Slide([ button [] [ str "this should move" ]])
        // AnimationsOnHover([
        //   AnimationsOnHover [ Html.span "Hover me!"]
        //   AnimationsOnHover [ Html.p "So smooth!" ]
        // ])
      )

    [<ReactComponent>]
    let Counter() = 
      let count, setCount = React.useState(0)
      
      Html.button [
        prop.text (sprintf "counter : %i" count)
        prop.onClick (fun _ -> setCount (count + 1))
      ]

    [<ReactComponent>]
    let BoringReactComponent() =
      Html.p "Just some react component that should render"

    [<ReactComponent>]
    let Root() = 
      Html.div [
        BoringReactComponent()
        //Counter2({ initCount = 0 }) :> ReactElement
        Counter3 ({| initCount = 0 |})
        // AnimationsOnHover [ Html.span "Hover me!"]
        // AnimationsOnHover [ Html.p "So smooth!" ]
        Animations()
      ]

    open Browser.Dom

    let view model dispatch =
      div [  ] [
        button [
          Style [ Margin "auto" ]
          OnClick(fun _ -> printfn "dispatching hoist"; dispatch Hoist)
        ] [ str "load" ]
        div [Id "reactRoot"] []
      ]

      //ReactDOM.render(Root(), document.getElementById "reactRoot")

    open Elmish

    let update msg mdl =
      printfn "in sliding update with msg %A" msg
      match msg with
      | Loading ->
        mdl, Cmd.none //Cmd.ofMsg Hoist
      | Hoist ->
        printfn "hoisting"
        let cmd = 
          Cmd.OfFunc.attempt (fun _ -> 
            ReactDOM.render(Root(), document.getElementById "reactRoot")
          ) () LoadError
        mdl, cmd
      | Loaded ->
        mdl, Cmd.none
      | LoadError exn ->
        printfn "Hoist error: %A" exn
        mdl, Cmd.none

  // module Elmish =
  //   open Fable.React
  //   open Fable.React.Props

  //   let frames = 300

  //   type Model = {
  //     ShouldSlide : bool
  //     Progress : int
  //   }

  //   let init() = {
  //     ShouldSlide = false
  //     Progress = 0
  //   }

  //   type Msg =
  //   | Enable
  //   | Disable

  //   let Pos x y =
  //     [
  //       CSSProp.Left x
  //       Top y
  //     ]

  //   let box color x y =
  //     div [
  //       Style [
  //         Height 50
  //         Width 50
  //         BackgroundColor color
  //         Border "1px solid black"
  //         yield! Pos x y
  //       ]
  //     ] []

  //   let view model dispatch =
  //     div [] [
  //       box "red" 300 300
  //       box "blue" 500 500
  //       button [OnClick (fun _ -> Enable |> dispatch )] [ str "Start" ]
  //     ]

  //   let update msg model =
  //     match msg with
  //     | Enable ->
  //       { model with ShouldSlide = true}
  //     | Disable ->
  //       { model with ShouldSlide = false }