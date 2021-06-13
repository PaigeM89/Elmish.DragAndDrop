# Getting Started


## Installing

    [lang=bash]
    paket install Elmish.DragAndDrop

## Setup

1. At the scope you'll define the `DragDropContext` (see next step), add a `DragAndDropModel` to your model, and a `DragAndDropMsg` to your messages.

```fsharp
    open Elmish.DragAndDrop

    type Model = {
      DragAndDrop : DragAndDropModel
      ContentMap : Map<ContentKey, ContentValue>
    }

    type Msg =
    | DndMsg of DragAndDropMsg
    | InputChange of elementId : string * newValue : string
```

You will need a way to determine which content to render based on which element Id is given. It is recommended to use a map for this, storing your content in an easily accessible (and easily modifiable) way.

2. Create a `DragDropContext` at the scope you want the user to be able to drag things. This does not have to be the scope they can drop things; for usability, consider putting this context towards the root of your application.

```fsharp
    let mappedMsg msg = DndMsg msg

    DragDropContext.context
      model.DragAndDrop
      (mappedMsg >> dispatch) 
      div [] [ dropAreaContent ]
```

3. Create your `Draggables`. These consist of 2 parts: (1) the `Draggable` itself, the element that will move around the screen and be moved to different locations in your collection(s); (2) the `DragHandle`, the part of the element your user will click on to do these actions. In many cases, the entire `Draggable` will be a drag handle, but if your elements contain things the user may want to interact with, such as an input box or a checkbox, then you'll want to use define a specific `DragHandle`.

The `Draggable` will need the `DragHandle` to be part of the elements given to it, or it can be built as a `DragHandle` (making the whole element a drag handle). If no drag handle is provided, the element will not be draggable, though it will still be movable based on other items moving it out of the way to accomodate it dropping. A `DragHandle` must have a reference to the `id` of the `Draggable`. 

Additionally, you'll want to configure your `DragAndDropConfig` at the scope of any specific drag & drop, to define additional styles or properties to put on your various drag & drop elements.

```fsharp
      let dragAndDropConfig = {
          DragAndDropConfig.Empty() with
              DraggedElementStyles = Some [
                  MarginLeft -130.
                  MarginTop -50.
                  Opacity 0.8
                  Position PositionOptions.Fixed
                  Cursor "grabbing"
                  Background "#00ffff"
              ]
              HoverPreviewElementStyles = Some [
                  Opacity 0.2
              ]
        }

    let createDragHandle dndModel draggableId dispatch handleContent : ReactElement =
        let handleId = draggableId + "-handle"
        ElementGenerator.Create handleId [] [] handleContent
        |> DragHandle.dragHandle dndModel draggableId (mappedMsg >> dispatch)

    let createDraggable dndModel draggableId dispatch handleContent content =
        let handle = createDragHandle dndModel draggableId dispatch handleContent
        ElementGenerator.Create draggableId [] [] [ handle; content ]
        |> Draggable.draggable dndModel dragAndDropConfig (mappedMsg >> dispatch)
```

4. Define one or more `DropArea` elements for your `Draggables` to live. 

```fsharp
    let draggables 
      let handleContent = p [] [ "Click here to drag!" ]
      model.DragAndDrop.ElementIds()
      |> List.concat // collect into a single list; this is just for example, you may need to keep distinct lists
      |> List.map (fun rootElementId ->
        // find the content based on the Id given by drag and drop. In this example, this returns pure content
        // In most scenarios, you'll store some object, and map that to content at this step.
        let content = model.ContentMap |> Map.find rootElementId
        createDraggable dndModel rootelementId dispatch handleContent content
      )
    let dropArea = DropArea.fromDraggables div [] draggables
```

With all that, you're good to go! There's a fair bit of setup, which arises from the amount of configurability this library aims to achieve.

See the Examples folder for complete, working examples of different types of setups.

### Element Generators

This is a building block used to specify all the ingredients of a `ReactElement`, so that additional styles or properties can be added as needed before it is turned into an element.

### Drop Areas As Buckets

Sometimes you want a drop area that accepts an item and invokes a function, but doesn't keep the item around. An example of this might be dragging an item to delete it, or dragging an item over some element to create some event (perhaps creating a new element in the process). To do this, use `DropArea.asBucket`, which accepts a function for `onHover` and `onDrop`, and will invoke those functions during a hover and a drop.

See the "Drag To Delete" example to see this in action.
