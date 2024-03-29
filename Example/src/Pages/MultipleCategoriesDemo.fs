namespace Pages

(*
  If you have more than 1 distinct Drag & Drop categories of things that should not interact with each other, then you'll want to configure
  different Categories as shown in this demo.
*)

module MultipleCategoriesDemo =
  open System
  open Fable.React
  open Fable.React.Props
  open Elmish
  open Elmish.DragAndDrop

  type Genre =
  | Black
  | Death

  type Band = {
    Id : Guid
    Name : string
    Genre : Genre
  } with
    static member Create name genre = {
      Id = Guid.NewGuid()
      Name = name
      Genre = genre
    }

  [<Literal>]
  let BlackMetalBandSorting = "black-metal-category"
  [<Literal>]
  let DeathMetalBandSorting = "death-metal-category"
  [<Literal>]
  let TopTenBandSorting = "top-ten-category"

  type Model = {
    DragAndDrop : DragAndDropModel
    //BlackMetalBandSorting : DragAndDropModel
    BlackMetalBands : Map<string, Band>
    //DeathMetalBandSorting : DragAndDropModel
    DeathMetalBands : Map<string, Band>
    //TopTenBandSorting : DragAndDropModel
    TopTenBands : Map<string, Band>
  } with
    static member Init() = 
      let blackMetalBands = [
        Band.Create "Summoning" Black
        Band.Create "Aosoth" Black
        Band.Create "Deathspell Omega" Black
        Band.Create "Mayhem" Black
        Band.Create "Woman is the Earth" Black
        Band.Create "Borkr" Black
        Band.Create "Hwwauoch" Black
        Band.Create "Gherzen" Black
        Band.Create "Audn" Black
        Band.Create "Mgla" Black
      ]
      let deathMetalBands = [
        Band.Create "Suffering Hour" Death
        Band.Create "Abyssal" Death
        Band.Create "Decapitated" Death
        Band.Create "Forsman" Death
        Band.Create "Of Feather And Bone" Death
        Band.Create "Ulcerate" Death
        Band.Create "Dragged Into Sunlight" Death
        Band.Create "Black Curse" Death
        Band.Create "Teitanblood" Death
        Band.Create "Nile" Death
      ]
      let topten =
        let top5BM = blackMetalBands |> List.take 5
        let top5DM = deathMetalBands |> List.take 5
        top5BM @ top5DM
      let dnd = DragAndDropModel.createWithCategoriesAndItems [
        BlackMetalBandSorting, [blackMetalBands |> List.map (fun b -> string b.Id)]
        DeathMetalBandSorting, [ deathMetalBands |> List.map (fun b -> string b.Id) ]
        TopTenBandSorting, [ topten |> List.map (fun b -> string b.Id) ]
      ]
      {
        //BlackMetalBandSorting = DragAndDropModel.createWithItems (blackMetalBands |> List.map (fun b -> b.Id |> string ))
        BlackMetalBands = blackMetalBands |> List.map (fun x -> string x.Id, x) |> Map.ofList
        //DeathMetalBandSorting = DragAndDropModel.createWithItems (deathMetalBands |> List.map (fun b -> b.Id |> string))
        DeathMetalBands = deathMetalBands |> List.map (fun x -> (string x.Id), x) |> Map.ofList
        //TopTenBandSorting = DragAndDropModel.createWithItems (topten |> List.map (fun b -> b.Id |> string))
        TopTenBands = topten |> List.map(fun b -> string b.Id, b) |> Map.ofList
        DragAndDrop = dnd
      }

  type Msg =
  | Init
  | DndMsg of DragAndDropMsg

  let dragAndDropConfig = {
    DragAndDropConfig.Empty() with 
      DraggedElementStyles = Some [
          MarginLeft -130.
          MarginTop -50.
          Opacity 0.8
          Position PositionOptions.Fixed
          Cursor "grabbing"
          Background "darkcyan"
      ]
      HoverPreviewElementStyles = Some [
        Opacity 0.2
        PointerEvents "None"
      ]
      MoveThrottleTimeMs = Some (System.TimeSpan.FromMilliseconds 100.)
  }

  let mappedMsg m = DndMsg m

  let renderDraggable model dispatch (band : Band) =
    match band.Genre with
    | Black ->
      Draggable.SelfHandle
        model.DragAndDrop
        BlackMetalBandSorting
        dragAndDropConfig
        (mappedMsg >> dispatch)
        (string band.Id)
        div
        [
          Cursor "grab"
          JustifyContent "center"
        ]
        [
          Id (string band.Id)
          ClassName "content-verysmall"
        ]
        [
          h3 [] [ str band.Name ]
          p [] [ str "Black metal" ]
        ]
    | Death ->
      Draggable.SelfHandle
        model.DragAndDrop
        DeathMetalBandSorting
        dragAndDropConfig
        (mappedMsg >> dispatch)
        (string band.Id)
        div
        [
          Cursor "grab"
          JustifyContent "center"
        ]
        [
          Id (string band.Id)
          ClassName "content-verysmall"
        ]
        [
          h3 [] [ str band.Name ]
          p [] [ str "Death metal" ]
        ]

  let renderTopBandDraggable model dispatch (band : Band) =
    let genre = 
      match band.Genre with
      | Black -> "Black metal"
      | Death -> "Death metal"
    Draggable.SelfHandle
      model.DragAndDrop
      TopTenBandSorting
      dragAndDropConfig
      (mappedMsg >> dispatch)
      (string band.Id)
      div
      [ 
        Cursor "grab"
        JustifyContent "center"
      ]
      [
        Id (string band.Id)
        ClassName "content-verysmall"
      ]
      [
        h3 [] [ str band.Name ]
        p [] [ str genre ]
      ]

  let blackMetalDropArea model dispatch =
    let bands =
      model.DragAndDrop.ElementIdsForCategory BlackMetalBandSorting
      |> List.collect snd
      |> List.map (fun (index, id) ->
        let element = model.BlackMetalBands |> Map.tryFind id
        element
      )
      |> List.choose id
    let draggables = bands |> List.collect (renderDraggable model dispatch)
    div [
      Id "black-metal-drop-area"
      Style [
        Display DisplayOptions.Grid
      ]
    ] [
      h2 [] [ str "Top 10 Black Metal Bands" ]
      p [] [ str "Drag to sort this entirely arbitrary list of top 10 black metal bands" ]
      yield! draggables
    ]
  
  let deathMetalDropArea model dispatch =
    let bands =
      model.DragAndDrop.ElementIdsForCategory DeathMetalBandSorting
      |> List.collect snd
      |> List.map (fun (index, id) ->
        let element = model.DeathMetalBands |> Map.tryFind id
        element
      )
      |> List.choose id
    let draggables = bands |> List.collect (renderDraggable model dispatch)
    div [
      Id "death-metal-drop-area"
      Style [
        Display DisplayOptions.Grid
      ]
    ] [
      h2 [] [ str "Top 10 Death Metal Bands" ]
      p [] [ str "Drag to sort this entirely arbitrary list of top 10 death metal bands" ]
      yield! draggables
    ]

  let topTenDropArea model dispatch =
    let bands =
      model.DragAndDrop.ElementIdsForCategory TopTenBandSorting
      |> List.collect snd
      |> List.map (fun (index, id) ->
        let element = model.TopTenBands |> Map.tryFind id
        element
      )
      |> List.choose id
    let draggables = bands |> List.collect (renderTopBandDraggable model dispatch)
    div [
      Id "top-metal-drop-area"
      Style [
        Display DisplayOptions.Grid
      ]
    ] [
      h2 [] [ str "Top 10 Bands" ]
      p [ ] [ str "Drag to sort the top 5 bands from each category" ]
      yield! draggables
    ]

  let view model dispatch =
    let bmDropArea = blackMetalDropArea model dispatch
    let dmDropArea = deathMetalDropArea model dispatch
    let topDropArea = topTenDropArea model dispatch
    let contextProps : IHTMLProp list = [
      Style [
        Background "#0066ff"
        Width "100%"
        Display DisplayOptions.Flex
      ]
      ClassName "page-small"
    ]
    DragDropContext.Context
      model.DragAndDrop
      (mappedMsg >> dispatch)
      div
      contextProps
      [
        bmDropArea
        dmDropArea
        topDropArea
      ]

  let update msg (model : Model) =
    match msg with
    | Init ->
      (Model.Init()), Cmd.none
    | DndMsg msg ->
      match msg with
      | OnDrag _ -> ()
      | _ -> printfn "Handling dnd msg %A" msg
      let dndMdl, cmd = dragAndDropUpdate msg model.DragAndDrop
      let model = { model with DragAndDrop = dndMdl }
      model, Cmd.map DndMsg cmd