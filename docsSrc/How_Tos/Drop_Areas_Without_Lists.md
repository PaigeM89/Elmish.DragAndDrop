# Drop Areas Without Lists / Drop Areas as Buckets

Sometimes you'll want to make a `DropArea` that doesn't show the items in it; you want a "bucket" to drop items into and have those items removed from the current list.

This is done with `DropArea.asBucket`, which has the following signature:

`DropArea.asBucket DragAndDropModel DragAndDropConfig (onHover : MouseEvent -> Id -> unit) (onDrop : MouseEvent -> ElementId -> unit) (dispatch : DragAndDropMsg -> unit) ElementGenerator`

The `model` and `config` are used to set state & rendering options, respecitvely.

`OnHover` is called when an element is hovered over the `DropArea`, and is given the mouseEvent & the _Id of the DropArea_. `OnDrop` is called when a drag is ended while hovering over a DropArea, and is given the mouseEvent & _the Id of the dropped element_. 

`dispatch` is the classic Elmish dispatch. `ElementGenerator` 