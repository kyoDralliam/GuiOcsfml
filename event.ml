


type event = 
  | Resize of float * float 
  | MouseEnter
  | MouseLeave
  | Pressed
  | Released
  | TextEntered of int
  | KeyPressed of OcsfmlWindow.KeyCode.t
