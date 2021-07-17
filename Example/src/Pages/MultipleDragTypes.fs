namespace Pages

(*
  This demo shows how to create not just multiple drag & drop categories (which is already shown in the
  MultiList Demo), but how to prevent elements from crossing into another category or another drop bucket.

  This demo also shows how to take things from an un-sortable category and drag them to a sortable one.
*)

open System

module MultipleDragTypesDemo =
  open Fable.React
  open Fable.React.Props
  open Elmish
  open Elmish.DragAndDrop

  let tryParseGuid str = 
    match Guid.TryParse str with
    | true, g -> Some g
    | false, _ -> None

  type Movie = {
    Id : Guid
    Title : string
    Director : string
  } with
    static member Create title director = {
      Id = Guid.NewGuid()
      Title = title
      Director = director
    }
  type Band = {
    Id : Guid
    Name : string
    Genre : string
  } with
    static member Create name genre = {
      Id = Guid.NewGuid()
      Name = name
      Genre = genre
    }
  type Food = {
    Id : Guid
    Name : string
  } with
    static member Create name = {
      Id = Guid.NewGuid()
      Name = name
    }

  // [<RequireQualifiedAccess>]
  // type Category =
  // | Pool
  // | Band
  // | Food
  // | Movie
  // | General

  [<RequireQualifiedAccess>]
  type Thing =
  | Movie of Movie
  | Band of Band
  | Food of Food
  with
    member this.Id = 
      match this with
      | Movie m -> m.Id
      | Band b -> b.Id
      | Food f -> f.Id

  // type GroupedThings = {
  //   Ungrouped : Thing list
  //   FavoriteBands : Band list
  //   FavoriteFoods : Food list
  //   FavoriteMovies : Movie list
  //   TopFavorites : Thing list
  // } with
  //   static member Empty() = {
  //     Ungrouped = []
  //     FavoriteBands = []
  //     FavoriteFoods = []
  //     FavoriteMovies = []
  //     TopFavorites = []
  //   }
  //   static member Create ungrouped bands foods movies top = {
  //     Ungrouped = ungrouped
  //     FavoriteBands = bands
  //     FavoriteFoods = foods
  //     FavoriteMovies = movies
  //     TopFavorites = top
  //   }
  //   static member UngroupedThings ug = { GroupedThings.Empty() with Ungrouped = ug }
  //   static member Bands b = { GroupedThings.Empty() with FavoriteBands = b }
  //   static member Foods f = { GroupedThings.Empty() with FavoriteFoods = f }
  //   static member Movies m = { GroupedThings.Empty() with FavoriteMovies = m }
  //   static member Top t = { GroupedThings.Empty() with TopFavorites = t }
  //   static member (+) (gt1, gt2) =
  //     {
  //       Ungrouped = gt1.Ungrouped @ gt2.Ungrouped
  //       FavoriteBands = gt1.FavoriteBands @ gt2.FavoriteBands
  //       FavoriteFoods = gt1.FavoriteFoods @ gt2.FavoriteFoods
  //       FavoriteMovies = gt1.FavoriteMovies @ gt2.FavoriteMovies
  //       TopFavorites = gt1.TopFavorites @ gt2.TopFavorites
  //     }

  type Model = {
    DragAndDrop : DragAndDropModel
    CategoryIds : (Guid * string) list
    Things : Map<Guid, Thing>
    FavoriteMovies : Map<Guid, Movie>
    FavoriteBands : Map<Guid, Band>
    FavoriteFoods : Map<Guid, Food>
    FavoriteThings : Map<Guid, Thing>
  } with
    static member Init() =
      let content = [
        Movie.Create "The Lord Of The Rings" "Peter Jackson" |> Thing.Movie
        Movie.Create "The Hobbit" "Peter Jackson" |> Thing.Movie
        Movie.Create "Saving Private Ryan" "Steven Spielberg"  |> Thing.Movie
        Movie.Create "The Matrix" "Lana & Lilly Wachowsky" |> Thing.Movie
        Movie.Create "Star Wars: Episode 5 - The Empire Strikes Back" "Irvin Kershner" |> Thing.Movie
        Band.Create "Summoning" "Atmospheric Black Metal" |> Thing.Band
        Band.Create "Decapitated" "Technical Death Metal" |> Thing.Band
        Band.Create "Auðn" "Black Metal" |> Thing.Band
        Band.Create "Deathspell Omega" "Black Metal" |> Thing.Band
        Band.Create "Mare Cognitum" "Black Metal" |> Thing.Band
        Food.Create "Pizza" |> Thing.Food
        Food.Create "Cheeseburger" |> Thing.Food
        Food.Create "Monster brand energy drinks" |> Thing.Food
        Food.Create "Old leather shoes" |> Thing.Food
        Food.Create "Burritos" |> Thing.Food
      ]
      let contentIds = content |> List.map (fun c -> c.Id |> string)

      {
        // todo: the items may not be recognizable from the start if we don't add them here, but
        // we don't care about dragging them in their source category.
        DragAndDrop = DragAndDropModel.createWithItems contentIds
        CategoryIds = []
        Things = content |> List.map (fun x -> x.Id, x) |> Map.ofList
        FavoriteMovies = Map.empty
        FavoriteBands = Map.empty
        FavoriteFoods = Map.empty
        FavoriteThings = Map.empty
      }

  module Model =
    let getThingById id (m : Model) = m.Things |> Map.tryFind id
    let getThingByIdR (m : Model) id = m.Things |> Map.tryFind id

  type Msg = 
  | Init
  | DndMsg of DragAndDropMsg
  | IsValidDrop of elementId : Guid * hoveredOverId : string
  | IsInvalidDrop of elementId : Guid * hoverOverId : string

  let mappedMsg m = DndMsg m

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
      MoveThrottleTimeMs = Some (System.TimeSpan.FromMilliseconds 500.)
  }

  let renderThing (thing : Thing) =
    let props : IHTMLProp list = [] 
    match thing with
    | Thing.Movie m ->
      [
        h2 props [ str m.Title ]
        h4 props [ str ("Directed by: " + m.Director)]
      ]
    | Thing.Band b ->
      [
        h2 props [ str b.Name ]
        h4 props [ str b.Genre ]
      ]
    | Thing.Food f ->
      [
        h2 props [ str f.Name ]
      ]

  let createDraggable model dispatch thingId =
    match Model.getThingById thingId model with
    | Some thing ->
      let content = renderThing thing
      let styles = [
        Cursor "grab"
        JustifyContent "center"
        Width "100%"
      ]
      let eg = ElementGenerator.Create (string thingId) styles [ ClassName "content-small" ] content
      // create the whole thing as a drag handle; there are no interactive elements to consider
      Draggable.asDragHandle model.DragAndDrop dragAndDropConfig dispatch eg
      |> Some
    | None -> None
  
  let createDraggableFromThing model dispatch thing =
    let content = renderThing thing
    let styles = [
      Cursor "grab"
      JustifyContent "center"
    ]
    let eg = ElementGenerator.Create (string thing.Id) styles [ ClassName "content-small" ] content
    // create the whole thing as a drag handle; there are no interactive elements to consider
    Draggable.asDragHandle model.DragAndDrop dragAndDropConfig dispatch eg
    |> Some

  let renderElementSourceCollection draggables =
    DropArea.fromDraggables div [] draggables

  let renderFavorite model dispatch title draggables =    
    let titleStatic =
      ElementGenerator.Create "" [] [] [ str title ]
      |> ElementGenerator.setTag h1
      |> Draggable.draggable model dragAndDropConfig dispatch
    let placeholder =
      let id = Guid.NewGuid() |> string
      ElementGenerator.Create id [] [] [ str "Drag here to place a favorite thing!" ]
      |> Draggable.draggable model dragAndDropConfig dispatch
    let styles = [ JustifyContent "center"; CSSProp.PaddingLeft "10px"; CSSProp.PaddingRight "10px" ]
    DropArea.fromDraggables div [ Id "drop-area"; Style styles ] (titleStatic :: (placeholder :: draggables))

  let renderCategoryAsDropBucket model dispatch title =
    let styles = [ 
      JustifyContent "center"
      CSSProp.PaddingLeft "10px"
      CSSProp.PaddingRight "10px"
      CSSProp.BackgroundColor "aquamarine" 
      CSSProp.MinWidth "50px"
    ]
    let title =
      ElementGenerator.Create "" styles [] [ str title ]
      |> ElementGenerator.setTag h1
    DropArea.asBucket model dragAndDropConfig (fun _ _ -> printfn "In drop bucket hover") (fun _ _ -> printfn "In drop bucket on drop") dispatch title

  let renderFavoriteBands model dispatch elements = 
    if elements |> List.isEmpty then
      let draggable =
        ElementGenerator.createGenerator
          "favorite-bands-placeholder" 
          [ Width "100%" ; Height "25px"; PaddingBottom "25px" ]
          []
          [ str "test" ]
        |> Draggable.draggable model dragAndDropConfig dispatch
      renderFavorite model dispatch "Top 3 Favorite Bands" [draggable]
    else
      renderFavorite model dispatch "Top 3 Favorite Bands" elements
  let renderFavoriteFoods model dispatch elements = renderFavorite model dispatch "Top 3 Favorite Foods" elements
  let renderFavoriteMovies model dispatch elements = renderFavorite model dispatch "Top 3 Favorite Movies" elements
  let renderTopFavorites model dispatch elements = renderFavorite model dispatch "Top 3 Favorite Things" elements

  let createBandDraggable model dispatch (id : Guid) (band : Band) =
    let styles = [
        Cursor "grab"
        JustifyContent "center"
        Width "100%"
      ]
    let props : IHTMLProp list = [ ClassName "content-small" ]
    let content = [
      h2 [] [ str band.Name]
      h4 [] [ str band.Genre ]
    ]
    Draggable.AsDraggable model dragAndDropConfig dispatch (string id) div styles props content

  let favoriteBandsList model dispatch categoryId content =
    let onHover : OnHover =
      fun ev draggableId dropAreaId throttleId ->
        match tryParseGuid draggableId with
        | Some draggableId ->
          match Model.getThingById draggableId model with
          | Some (Thing.Band _) -> IsValidDrop (draggableId, dropAreaId) |> dispatch
          | _ -> IsInvalidDrop (draggableId, categoryId) |> dispatch
        | None -> ()
    let onDrop  : OnDrop =
      fun ev draggableId dropAreaId ->
        match tryParseGuid draggableId with
        | Some draggableId -> 
          match Model.getThingById draggableId model with
          | Some (Thing.Band _) -> IsValidDrop (draggableId, dropAreaId) |> dispatch
          | _ -> IsInvalidDrop (draggableId, categoryId) |> dispatch
        | None -> ()
    let styles = [ JustifyContent "center"; CSSProp.PaddingLeft "10px"; CSSProp.PaddingRight "10px" ]
    let id = "band-drop-area"
    let props : IHTMLProp list = [ Id id; Style styles ]

    DropArea.Collection model.DragAndDrop dragAndDropConfig (mappedMsg >> dispatch) onHover onDrop id div props content

  let view model dispatch =
    let dndDispatch = mappedMsg >> dispatch

    let things =
      model.DragAndDrop.ElementIds()
      |> List.map (fun li -> li |> List.choose tryParseGuid)
      |> List.map (fun li -> li |> List.choose (Model.getThingByIdR model))

    let sourceElements =
      //groupedElements.Ungrouped
      things
      |> List.tryItem 0
      |> Option.defaultValue []
      |> List.choose (createDraggableFromThing model dndDispatch)
      |> renderElementSourceCollection

    let bandElements = 
      things
      |> List.tryItem 1
      |> Option.defaultValue []
      |> List.choose (fun t ->
        match t with
        | Thing.Band b -> Some b
        | _ -> None
      )
      |> List.collect (createBandDraggable model.DragAndDrop dndDispatch (Guid.NewGuid()))
      |> favoriteBandsList model dispatch "favorite-bands"

    let content =
      div [] [
        sourceElements
        div [
          Style [
            Display DisplayOptions.Flex
          ]
        ] [
          bandElements
        ]
      ]
    
    let contextProps : IHTMLProp list = [
      Style [
        Background "#0066ff"
        Width "100%"
        Display DisplayOptions.Flex
        // JustifyContent "center"
      ]
      ClassName "page-small"
    ]
    DragDropContext.context model.DragAndDrop dndDispatch div contextProps [content]

  let update msg model =
    match msg with
    | Init ->
      model, Cmd.none
    | DndMsg msg ->
      let dndModel, cmd = dragAndDropUpdate msg model.DragAndDrop
      { model with DragAndDrop = dndModel }, Cmd.map DndMsg cmd
    | IsValidDrop(elementId, hoveredOverId) -> failwith "Not Implemented"
    | IsInvalidDrop(elementId, hoverOverId) -> failwith "Not Implemented"
