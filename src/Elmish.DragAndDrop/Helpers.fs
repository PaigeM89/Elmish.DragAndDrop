namespace DragAndDrop

module internal List =
  open System

  let private len x = List.length x

  let tryTake n li =
    if len li < n then
      li
    elif n < 0 then
      []
    else
      List.take n li

  let removeAt index li =
    if len li < index then
      li
    elif index = 0 then
      List.tail li
    else
      let h = tryTake (index) li
      let t = List.skip (index + 1) li
      h @ t

  let insertAt item index li =
    if len li <= index then
      li @ [item]
    elif index <= 0 then
      item :: li
    else
      let h = tryTake (index) li
      let t = List.skip (index) li
      h @ [item] @ t


  let replaceAt item index li =
    if len li <= index then
      li @ [ item ]
    elif index = 0 then
      let t = List.tail li
      item :: t
    else
      let h = tryTake (index) li
      let t = List.skip (index + 1) li
      h @ [item] @ t
