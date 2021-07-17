Notes

# GOAL

I want to make THIS:

https://react-beautiful-dnd.netlify.app/?path=/story/board--simple

https://github.com/atlassian/react-beautiful-dnd/blob/master/stories/src/board/board.jsx
https://github.com/atlassian/react-beautiful-dnd/blob/master/stories/src/board/column.jsx


Multiple categories
Categories can be sorted
Categories have a static title (static element)
Elements take up a variable amount of vertical space
  This example has fixed width
Elements are sortable
Elements can move between categories
  one thing to add: elements should be able to be blocked from categories
Source category can be styled when the element leaves that source
Hovering over the _title_ of a category lets you add to the _content_ of that category
  essentially, the hover listener doesn't have to be tied to the drop area - it can be tied to a parent.

# Core Concepts

## Add More or Add Less?

The core design issue I keep sort of running into is that I don't feel like I have a good way to expand functionality on existing components. I keep creating new types & structures, which take a LOT of design work to feel elegant, and require a LOT of documentation. If I can find a way to instead use regular HTML elements, just appending some stuff to them, then I think that would be idea.

I think the example project uses react components, so it's essentially using inheritance. I wonder if that means my approach is still ok over all?

My approach is essentially types, but done in a way that you _have_ to supply all the required fields.

## The drag context
  This is the root level thing that lets us drag items anywhere. It doesn't do much other than track mouse location & let the action be smooth & intuitive. Though it DOES allow different "drop areas" to be connected

  Ideas going forward:
  None. I think this part of the concept is fine.

## The Drop Area(s)
  These are the places you can drop items. Importantly, they are _only_ sortable items. Anything else goes OUTSIDE the drop area. I think this is causing a couple of issues.

  In the example, you can drag an item over a title, and that item will go to the first spot in that actual drop area (the title doesn't shift).
  
  In the example, an empy Drop Area retains a minimum height. This allows items to be dropped into that area.

  Ideas going forward:
  * Flesh out Drop Area as a full type, maybe?

# Current State

I use `ElementGenerators` so I can build elements & copy them with added styles & properties.

Why do I defer rendering? What happens if I don't?

# PATH

## Listeners

It all starts here, the things that listen for events.

## Drag Listener

I need things that listen for a drag, then drag an element.

## Draggable

The thing that moves. This needs styles & props separate so I can render preview elements & similar concepts.

## Hover Listener

Listens for when a draggable has entered its territory.

This is also probably going to be how we solve the "insert at the bottom" issue.

## Drop Area

The place where things actually go when released (sorted).

# Element Gen vs. Other Record vs. HTML-style Props vs. huge funcs

Element gen is very clunky to use and has no synergy with other stuff, and can often encourage or require  additional nesting.

Any other record will be similiarly clunky, as well as hurting backwards compatibility.

HTML Style props will run the risk of missing vital props.

Huge funcs is just annoying.

what do