let default_background_color = ref (OcsfmlGraphics.Color.rgb 255 255 255)

class panel 
  ?(geometry = Geometry.create (0.,0.) (10., 10.))
  ?(child = Widget.empty_widget)
  ?(dragable = false)
  ?(background_color = !default_background_color)
  () =
object (self)
  inherit Widget.widget geometry as super
  val mutable child = child
  val mutable dragable = dragable
  val mutable background_color = background_color


  method draw : 'a . (#OcsfmlGraphics.render_target as 'a) -> unit =
    fun target ->
      let fill_color = background_color in
      let position = Geometry.position geometry in
      let size = Geometry.size geometry in
      let shape = new OcsfmlGraphics.rectangle_shape
        ~position ~size ~fill_color ()
      in
      target#draw shape ;
      child#draw target


  method update_geometry area = 
    super#update_geometry area ;
    child#update_geometry area

  method set_dragable b = dragable <- b 


  method private on_event event = 
    match event with 
      | Event.Pressed -> self#listen_mouse_moves true ; false
      | Event.Released -> self#listen_mouse_moves false ; false
      | Event.MouseMoved (delta_x, delta_y) ->
          let area = { geometry with 
            Geometry.x = geometry.Geometry.x +. delta_x ; 
            Geometry.y = geometry.Geometry.y +. delta_y 
          } in
          self#update_geometry area ;
          true
      | _ -> false
   
end
