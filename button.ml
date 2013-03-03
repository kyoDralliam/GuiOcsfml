open Widget


class button (x',y',w,h) pressedCbk =
object
  inherit widget

  val x = x'
  val y = y'
  val width = w
  val height = h
  val buttonPressed = pressedCbk

  val onMouseClick (mX,mY) =
    if   x < mX && (x +. width) > mX
      && y < mY && (x +. height) > mY
    then (buttonPressed () ; true)
    else false

  method! handleEvent ev =
    match ev with
      | MouseClick pos -> onMouseClick pos
      | _ -> false

end
