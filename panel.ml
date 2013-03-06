class panel 
  ?(geometry=Geometry.create (0.,0.) (10., 10.))
  ?(child=Widget.empty_widget)
  ?(dragable=false)
  () =
object (self)
  inherit Widget.widget geometry as super
  val mutable child = child
  val mutable dragable = dragable

  method update_geometry area = 
    super#update_geometry area ;
    child#update_geometry area

  method set_dragable b = dragable <- b 

  method handle_event event = 
    match event with 
      | Event.Pressed -> 
          self#listen_mouse_moves true ;
          child#handle_event event 
      | Event.Released -> 
          self#listen_mouse_moves false ;
          child#handle_event event 
      | _ -> child#handle_event event 


  method private on_event = child#on_event
end
