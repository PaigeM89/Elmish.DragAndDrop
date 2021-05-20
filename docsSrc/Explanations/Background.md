# Background

`Elmish.DragAndDrop` is designed to provide a simple, intuitive drag & drop functionality for the end user. Compared to plain javascript and/or pure DOM fiddling, this is more complicated using an Elmish architecture, as everything must take place inside the MVU loop and must be as agnostic as possible about the actual rendered elements on the page (though this is not fully achieved - Sliding, for example, calculates the slide based off the actual rendered element's position).

This library is designed to have minimal extra render loops. Mouse events should only be added to elements when those events are appropriate to fire, so moving the mouse should only send a `OnDrag <mouse coordinates>` event if the user is currently dragging something.

This library is built around having a collection of items, or multiple collections, and the user wanting to re-order or otherwise change the sorting of those items. Other possible uses for drag & drop, like moving elements on a canvas, fall outside the scope of this library.

