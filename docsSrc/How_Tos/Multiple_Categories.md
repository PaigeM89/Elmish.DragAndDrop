# Multiple Categories

If you have multiple categories and you want to drag & drop elements between those categories, simply change the `dragAndDropCategoryKey` passed in when building a `Draggable` and a `DropArea`:

```
DragHandle.Handle Handle model categoryKey draggableId dispatch tag props content 

Draggable.SelfHandle model categoryKey config dispatch id tag styles props content

Draggable.InnerHandle model categoryKey config dispatch id tag styles props content

DropArea.DropArea model categoryKey config mouseEventHandlers dispatch id tag props content
```

Note that the `DragAndDropModel` and the `DragDropContext` do not need to specify the category. They both work for items in _every_ category; that is, they should be shared for every `DragAndDrop` instance in your application, and use the category to know when elements should not interact with each other.