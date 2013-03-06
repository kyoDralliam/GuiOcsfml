module FocusWitness =
struct
  type t = unit
  let focus_witness : t = ()
end

class virtual widget g = 
object(self)
  val mutable geometry : Geometry.t = g
  val mutable hovered = false
                    
  method update_geometry area =
    geometry <- area
  method resize size =
    self#update_geometry (Geometry.create (0.,0.) size)



  method virtual draw : 'a . (#OcsfmlGraphics.render_target as 'a) -> unit
  method private virtual on_event : Event.event -> bool

  method gain_focus : FocusWitness.t -> unit = fun _ -> ()

  method lose_focus : FocusWitness.t -> unit = fun _ -> ()

  method handle_event sfev =
    OcsfmlWindow.Event.( match sfev with
      | MouseMoved { x ; y } ->
          if hovered && not (Geometry.isInRect (float x, float y) geometry)
          then (hovered <- false ; self#on_event Event.MouseLeave)
          else if not hovered && Geometry.isInRect (float x, float y) geometry
          then (hovered <- true  ; self#on_event Event.MouseEnter)
          else false
      | MouseButtonPressed (_, { x ; y }) ->
          Geometry.isInRect (float x, float y) geometry
          && self#on_event Event.Pressed
      | MouseButtonReleased _ ->
          self#on_event Event.Released
      | TextEntered { unicode } -> 
          self#on_event (Event.TextEntered unicode)
      | _ -> false
    )

end

let empty_widget =
object
  inherit widget (Geometry.create (0.,0.) (0.,0.))

  method private on_event _ = false
  method draw _ = ()
end

module Focus =
struct
      
  let focused_widget_opt : widget option ref = ref None
  let focused_widget () = 
    !focused_widget_opt

  let grab_focus (widget : #widget) = 
    begin match !focused_widget_opt with 
      | None -> ()
      | Some focused_widget -> 
          focused_widget#lose_focus FocusWitness.focus_witness
    end;
    focused_widget_opt := Some widget;
    widget#gain_focus FocusWitness.focus_witness;
    true

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
