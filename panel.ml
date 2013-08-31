open OcsfmlGraphics

let default_background_color = ref (Color.rgb 255 255 255)

module PanelAttribute =
struct
  let background_color = Theme.Attribute.Identifier.create ()
end

class panel 
  ?(geometry = Geometry.create (0.,0.) (10., 10.))
  ?(child  = Widget.empty_widget)
  ?(dragable = false)
  theme_id =
object (self)
  inherit Widget.widget geometry as super
  val mutable child = child
  val mutable dragable = dragable
  val theme_id = theme_id
  initializer self#update_geometry geometry

  method draw : 
    'a . (#render_target as 'a) -> Theme.Set.t -> unit =
    fun target themeset ->
      let theme = Theme.Set.find themeset theme_id in
      let fill_color = Theme.get theme 
        PanelAttribute.background_color Theme.Color in
      let position = Geometry.position geometry in
      let size = Geometry.size geometry in
      let shape = new rectangle_shape
        ~position ~size ~fill_color ()
      in
      target#draw shape ;
      child#draw target themeset


  method update_geometry area = 
    super#update_geometry area ;
    child#update_geometry area

  method set_dragable b = dragable <- b 

  method handle_event sfev = 
    child#handle_event sfev || super#handle_event sfev

  method private on_event event = 
    match event with 
      | Event.Pressed -> 
          dragable && (self#listen_mouse_moves true ; true)
      | Event.Released -> 
          dragable && (self#listen_mouse_moves false ; true)
      | Event.MouseMoved (delta_x, delta_y) ->
          let area = { geometry with 
            Geometry.x = geometry.Geometry.x +. delta_x ; 
            Geometry.y = geometry.Geometry.y +. delta_y 
          } in
          self#update_geometry area ;
          false
      | _ -> false
   
end
