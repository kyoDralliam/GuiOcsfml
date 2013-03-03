class virtual extern_widget = object
  method virtual draw : 'a . (#OcsfmlGraphics.render_target as 'a) -> unit
  method virtual update_geometry : Geometry.t -> unit
  method virtual geometry : Geometry.t
  method virtual onEvent : Event.event -> bool
end
  
type widget = 
  | Label of Label.t
  | Button of Button.t * widget
  | Extern of extern_widget


let rec draw widget (target:#OcsfmlGraphics.render_target) = 
  match widget with
    | Label lab -> Label.draw lab target
    | Button (b, child) -> Button.draw b target draw child
    | Extern e -> e#draw target


let rec update_geometry widget area =
  match widget with
    | Label lab -> Label.update_geometry lab area
    | Button (b, child) -> Button.update_geometry b area update_geometry child 
    | Extern e -> e#update_geometry area

let resize widget size = update_geometry widget (Geometry.create (0.,0.) size)
        
let geometry = function
  | Label lab -> Label.geometry lab
  | Button (b, _) -> Button.geometry b
  | Extern e -> e#geometry

let rec handle_mouse_event widget ev pos =
  if Geometry.isInRect pos (geometry widget)
  then (match widget with
    | Label lab -> Label.onEvent lab ev
    | Button (b, child) ->
        handle_mouse_event child ev pos ||  Button.onEvent b ev
    | Extern e -> e#onEvent ev
  )
  else false

let handle_event widget ev =
  OcsfmlWindow.Event.(match ev with
    | MouseButtonReleased (_, { x ; y }) ->
        handle_mouse_event widget Event.Click (float x, float y)
    | _ -> false
  )
