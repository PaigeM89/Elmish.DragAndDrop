# Things to do

Contributions are welcome!

This file tracks things that need to be done (obviously). I used to track this in the README but that got too busy. I will make github issues for these at some point, I'm sure.

## Feature TODOs

* Sliding animations
  * The moving elements that adjust to make space for the draggable should slide out of the way
  * Don't forget to make a demo when it's done!
* Allow drop areas to also be draggables
  * I think I need to really do a big think about the models & how they interact

## Bugs


* Fix a bug with multi-list inserting where inserting to the bottom of a list requires inserting into the middle first. This will probably require a ghost last element to hover over, which should disappear. If this approach is used, that element will need to dynamically take up the rest of the space in that list.
  * Note that this bug means that empty categories can't be dropped into.
  * This bug was not yet fixed in the rewrite.
    * While we can determine when a placeholder is hovered over (via ID magic), we don't have any other useful infomration. We _could_ rip out the drop area ID from the name (and at this point entirely take over ID'ing this element), but we don't track drop area IDs, either. We _could_. 
* Fix bug with flickering on large items as they slide back & forth from a spot. This is mitigated by throttling, so less of a high priority, but it'd be nice to resolve it completely if it's possible without being messy.
  * Note that throttling has weird interactions when dragging over other elements, too, since the whole interaction is throttled. Maybe this is a small tweak?
* Use location finding & offset calculations to place a dragged item under the cursor at the spot it was clicked; right now, all dragged items appear in the same spot under the cursor, regardless of where the user clicked on that item.
* Fix bug where "grabbing" cursor never appears, though "grab" does.
* Fix a bug where the correct setup of padding & margins & drag handles & draggables creates areas you can click / have a grab cursor but don't actually hit the handle and thus don't drag.
  * current, MultipleDragTypesDemo has this bug on some of the elements.

## Documentation TODOs

* Always room for more!
* Host docs on github
* Lots of room to add more XML comments in the code!

## Demos

* Horizontal elements
  * This isn't really conceptually different from what is already there, just a lot of CSS fighting,
    but it's a good reference anyways.
* Grid Demo
  * This may not work off the bat. A 3x3 grid to make some kind of sliding puzzle would be neat, though.

