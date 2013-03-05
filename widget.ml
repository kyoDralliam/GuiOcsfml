class virtual widget g = 
object(self)
  val mutable geometry : Geometry.t = g
                    
  method update_geometry area =
    geometry <- area
  method resize size =
    self#update_geometry (Geometry.create (0.,0.) size)


  method virtual draw : 'a . (#OcsfmlGraphics.render_target as 'a) -> unit
(*  method virtual onEvent : Event.event -> bool *)
  method virtual onEvent : OcsfmlWindow.Event.t -> bool
end

let empty_widget =
object
  inherit widget (Geometry.create (0.,0.) (0.,0.))

  method onEvent _ = false
  method draw _ = ()
end


(*
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
*)
