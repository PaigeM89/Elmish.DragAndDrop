# Structure

## Concept

The basic design is pretty simple:

* If a user is not dragging anything, then all `Draggable` elements should have an `OnMouseDown` event handler to send a message if they are clicked (indicating the user is dragging that item).
* While the mouse is held down, the drag is Active, and:
  * No elements listen for `OnMouseDown`, because the mouse is already down
  * Elements listen for `OnMouseEnter` to know if the user dragged the moving element over that element
  * The `DropArea` listens for `OnMouseUp`, and ends the drag with the items in the current location if the mouse button is released.

During a drag, a preview is rendered so the user can see exactly what releasing the mouse will do.

Eventually Someday (TM), sliding will be added, making it clearer which elements moved where to accomodate an element drop preview.

## Implementation

### Drag Handles

A `DragHandle` is the actual element the user can click. It will refer to the _actual element to be dragged_, which can be an element above itself (consider the use case of a symbol - like your favorite Font Awesome icon - being used as a "handle" to drag a larger element that it is part of). Note that a `DragHandle` is the thing you actually implement in your code; a `Draggable` is a concept, but not an object in the library. [^1]

It's very likely you'll want to add some additional styling to dragged or preview elements, which is provided via the `DragAndDropConfig` object. This lets you configure additional styles & properties for different elements, such as reducing opacity slightly for the dragged element or giving the preview element a slightly different background color. See the examples for some ideas. Styles (`CSSProp` lists) are configured separately because the dragged element needs to be shifted by the mouse's coordinates; if only lists of `IHTMLProps` were used, it would not be possible to add these styles without overriding other styles.

### Drop Areas

Collections of `Draggables` are put into `DropAreas`, which listen for some mouse events (such as releasing a dragged item), sort items on drop, and handle rendering for the actual `Draggable` elements using the `DragAndDropConfig`. 

A `DropArea` expects a collection of `ElementGenerator` in which every element either is or contains a `DragHandle`. It will check the current drag state & the id of the element to determine what to actually render. This uses a record to generate the draggable element so that the direct sibling `divs` in the collection will be easier to style. If the `DropArea` took a collection of `ReactElement`, as it did in earlier designs, then the elements would have to be wrapped in a div (since you can't modify an existing element's styles or properties), and then it becomes significantly harder to style the list the way you want. 

## Footnotes

[^1]. It makes a bit more sense, from an intuitive API angle, to create a `Draggable` as an object that defines a `DragHandle`. I want to try this design in the future. Currently, I'm not sure how much it adds, and I don't want to change the design at a moment that it's stable.