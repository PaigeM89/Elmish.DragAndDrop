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

A `DragHandle` is the actual element the user can click. It will refer to the _actual element to be dragged_, which can be an element above itself (consider the use case of a symbol - like your favorite Font Awesome icon - being used as a "handle" to drag a larger element that it is part of).

It's very likely you'll want to add some additional styling to dragged or preview elements, which is provided via the `DragAndDropConfig` record. This lets you configure additional styles & properties for different elements, such as reducing opacity slightly for the dragged element or giving the preview element a slightly different background color. See the examples for some ideas. Styles (`CSSProp` lists) are configured separately because the dragged element needs to be shifted by the mouse's coordinates; if only lists of `IHTMLProps` were used, it would not be possible to add these styles without overriding other styles.

### Drop Areas

Collections of `Draggables` are put into `DropAreas`, which listen for some mouse events (such as releasing a dragged item), sort items on drop, and handle rendering for the actual `Draggable` elements using the `DragAndDropConfig`. A `DropArea`  will check the current drag state & the id of the element to determine what to actually render; for example, the dragged element will render both the hovering element and the preview element.
