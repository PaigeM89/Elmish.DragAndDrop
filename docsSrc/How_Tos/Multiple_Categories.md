# Multiple Categories

If you have multiple categories and you want to drag & drop elements between those categories, simply send multiple lists to the `DragAndDrop` model.

```Fsharp
    let content = [
      [ "item 1"; "item 2"; "item 3" ]
      [ "item 4"; "item 5" ]
      [ "item 6"]
    ]
    /// create a drag and drop model with 3 categories
    let dndModel = DragAndDropModel.createWithItemsMultiList content
```

Note that you'll need to define multiple `DropAreas` for these elements to live in. If possible, it's recommended to dynamically generate the drop areas based on the ElementIds given:

```Fsharp
    model.DragAndDrop.ElementIds()
    |> List.mapi(fun index li ->
      let props = if index % 2 = 0 then leftDropAreaProps else rightDropAreaProps
      li
      |> List.map (fun id ->
        createDraggable model id dispatch
      )
      |> DropArea.fromDraggables div props
    )
```

This, however, comes at the cost of losing categories when they lose all their elements. This could potentially even leave you with only 1 category.

# Exluding Items From Categories

There is a WIP of this concept in `MultipleDragTypes.fs`.