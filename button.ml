type state = Normal | Clicked

class button ?(onClick=(fun _ -> ())) child =
object(self)
  inherit Widget.widget (Geometry.create (0.,0.) (50.,30.)) as super
  val mutable state = Normal
  val mutable onClick = onClick
  val child : Widget.widget = child

  val color = 
    fun state hovered ->
      match state with
        | Normal -> 
            if hovered
            then OcsfmlGraphics.Color.rgb 20 50 20
            else OcsfmlGraphics.Color.rgb 30 80 30
        | Clicked -> OcsfmlGraphics.Color.rgb 5 30 5
            

(*let get_child_texture btt render_func child =
  match btt.draw_cache with
    | Some rdr_texture -> rdr_texture#get_texture
    | None -> 
        let (w,h) = Geometry.size btt.geometry in
        let target = 
          let wi,hi = int_of_float w, int_of_float h in
          new OcsfmlGraphics.render_texture wi hi 
        in
        target#clear ~color:(color btt) () ;
        render_func child target ;
        target#display ;        
        btt.draw_cache <- Some target ;
        target#get_texture *)



  method draw target =
    let fill_color = color state hovered in
    let position = Geometry.position geometry in
    let size = Geometry.size geometry in
    let shape = new OcsfmlGraphics.rectangle_shape
      ~position ~size ~fill_color ()
    in
    target#draw shape ;
    child#draw target

  method press = state <- Clicked
  method release = state <- Normal ; onClick ()

  (*  let texture = get_child_texture btt render_func child in
      let sprite = new OcsfmlGraphics.sprite ~texture ~position () in
      let clipping_shape = new OcsfmlGraphics.rectangle_shape
      ~position ~size 
      ~fill_color:(OcsfmlGraphics.Color.rgba 255 255 255 255) ()
      in
      target#draw clipping_shape ;
      target#draw ~blend_mode:OcsfmlGraphics.BlendMultiply sprite
*)
    
  method update_geometry area =
    super#update_geometry area ;
    let x,y = Geometry.position area in
    let w,h = Geometry.size area in
    child#update_geometry 
      (Geometry.create (x +. 5.,y +. 5.) (w -. 10., h -. 10.))
      

  method private on_event ev =
    Event.(
      match ev with
        | Pressed -> (self#press; true)
        | Released -> 
            state = Clicked && (self#release; true)
        | _ -> false
    )
end
