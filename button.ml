module ButtonAttribute =
struct
  let use_texture = Theme.Attribute.Identifier.create ()

  (* Don't use texture *)
  let color_normal  = Theme.Attribute.Identifier.create ()
  let color_hovered = Theme.Attribute.Identifier.create ()
  let color_clicked = Theme.Attribute.Identifier.create ()

  (* Use texture *)
  let upper_left_normal  = Theme.Attribute.Identifier.create ()
  let lower_left_normal  = Theme.Attribute.Identifier.create ()
  let upper_right_normal = Theme.Attribute.Identifier.create ()
  let lower_right_normal = Theme.Attribute.Identifier.create ()
  let top_normal         = Theme.Attribute.Identifier.create ()
  let bottom_normal      = Theme.Attribute.Identifier.create ()
  let left_normal        = Theme.Attribute.Identifier.create ()
  let right_normal       = Theme.Attribute.Identifier.create ()
  let center_normal      = Theme.Attribute.Identifier.create ()


  let upper_left_hovered  = Theme.Attribute.Identifier.create ()
  let lower_left_hovered  = Theme.Attribute.Identifier.create ()
  let upper_right_hovered = Theme.Attribute.Identifier.create ()
  let lower_right_hovered = Theme.Attribute.Identifier.create ()
  let top_hovered         = Theme.Attribute.Identifier.create ()
  let bottom_hovered      = Theme.Attribute.Identifier.create ()
  let left_hovered        = Theme.Attribute.Identifier.create ()
  let right_hovered       = Theme.Attribute.Identifier.create ()
  let center_hovered      = Theme.Attribute.Identifier.create ()


  let upper_left_clicked  = Theme.Attribute.Identifier.create ()
  let lower_left_clicked  = Theme.Attribute.Identifier.create ()
  let upper_right_clicked = Theme.Attribute.Identifier.create ()
  let lower_right_clicked = Theme.Attribute.Identifier.create ()
  let top_clicked         = Theme.Attribute.Identifier.create ()
  let bottom_clicked      = Theme.Attribute.Identifier.create ()
  let left_clicked        = Theme.Attribute.Identifier.create ()
  let right_clicked       = Theme.Attribute.Identifier.create ()
  let center_clicked      = Theme.Attribute.Identifier.create ()

  let texture_normal  = Theme.Attribute.Identifier.create ()
  let texture_hovered = Theme.Attribute.Identifier.create ()
  let texture_clicked = Theme.Attribute.Identifier.create ()

  let border_top    = Theme.Attribute.Identifier.create ()
  let border_bottom = Theme.Attribute.Identifier.create ()
  let border_left   = Theme.Attribute.Identifier.create ()
  let border_right  = Theme.Attribute.Identifier.create ()

  type texture_coords = {
    upper_left: int * int * int * int ;
    lower_left: int * int * int * int ;
    upper_right: int * int * int * int ;
    lower_right: int * int * int * int ;
    top: int * int * int * int ;
    bottom: int * int * int * int ;
    left: int * int * int * int ;
    right: int * int * int * int ;
    center: int * int * int * int
  }
  let setup_textured_button th ~normal ~hovered ~clicked image =
    let setup_one_part theme (left,top,width,height) id =
      Theme.add_texture theme id
        (image, OcsfmlGraphics.({ left ; top ; width ; height }))
    in
    let setup_parts theme coords (ul,ll,ur,lr,t,b,l,r,c) =
      let theme = setup_one_part theme coords.upper_left ul in
      let theme = setup_one_part theme coords.lower_left ll in
      let theme = setup_one_part theme coords.upper_right ur in
      let theme = setup_one_part theme coords.lower_right lr in
      let theme = setup_one_part theme coords.top t in
      let theme = setup_one_part theme coords.bottom b in
      let theme = setup_one_part theme coords.left l in
      let theme = setup_one_part theme coords.right r in
      let theme = setup_one_part theme coords.center c in
      theme
    in
    let theme = th in
    let theme = setup_parts theme normal 
      (upper_left_normal,lower_left_normal,
       upper_right_normal,lower_right_normal,
       top_normal,bottom_normal,left_normal,
       right_normal,center_normal) in
    let theme = setup_parts theme hovered 
      (upper_left_hovered,lower_left_hovered,
       upper_right_hovered,lower_right_hovered,
       top_hovered,bottom_hovered,left_hovered,
       right_hovered,center_hovered) in
    let theme = setup_parts theme clicked 
      (upper_left_clicked,lower_left_clicked,
       upper_right_clicked,lower_right_clicked,
       top_clicked,bottom_clicked,left_clicked,
       right_clicked,center_clicked) in
    theme

  let setup_defaults theme =
    let theme = Theme.add_bool theme
      use_texture false in
    let theme = Theme.add_color theme
      color_normal (OcsfmlGraphics.Color.rgb 20 50 20) in
    let theme = Theme.add_color theme
      color_hovered (OcsfmlGraphics.Color.rgb 30 80 30) in
    let theme = Theme.add_color theme
      color_clicked (OcsfmlGraphics.Color.rgb 5 30 5) in
    theme

end


type state = Normal | Clicked
type button_texture = {
  l: OcsfmlGraphics.texture;
  r: OcsfmlGraphics.texture;
  b: OcsfmlGraphics.texture;
  t: OcsfmlGraphics.texture;
  ul: OcsfmlGraphics.texture;
  ll: OcsfmlGraphics.texture;
  ur: OcsfmlGraphics.texture;
  lr: OcsfmlGraphics.texture;
  c: OcsfmlGraphics.texture
}


class button ?(onClick=(fun _ -> ())) theme child =
object(self)
  inherit Widget.widget (Geometry.create (0.,0.) (50.,30.)) as super
  val mutable state = Normal
  val mutable onClick = onClick
  val child : Widget.widget = child
  val theme_id = theme

  val color = 
    fun state hovered theme ->
      match state with
        | Normal -> 
            if hovered
            then Theme.get_color theme ButtonAttribute.color_hovered
            else Theme.get_color theme ButtonAttribute.color_normal
        | Clicked -> Theme.get_color theme ButtonAttribute.color_clicked

  val normal_texture =
    fun theme ->
      {
        l  = Theme.get_texture theme ButtonAttribute.left_normal ;
        r  = Theme.get_texture theme ButtonAttribute.right_normal ;
        b  = Theme.get_texture theme ButtonAttribute.bottom_normal ;
        t  = Theme.get_texture theme ButtonAttribute.top_normal ;
        ul = Theme.get_texture theme ButtonAttribute.upper_left_normal ;
        ll = Theme.get_texture theme ButtonAttribute.lower_left_normal ;
        ur = Theme.get_texture theme ButtonAttribute.upper_right_normal ;
        lr = Theme.get_texture theme ButtonAttribute.lower_right_normal ;
        c  = Theme.get_texture theme ButtonAttribute.center_normal
      }

  val hovered_texture =
    fun theme ->
      {
        l  = Theme.get_texture theme ButtonAttribute.left_hovered ;
        r  = Theme.get_texture theme ButtonAttribute.right_hovered ;
        b  = Theme.get_texture theme ButtonAttribute.bottom_hovered ;
        t  = Theme.get_texture theme ButtonAttribute.top_hovered ;
        ul = Theme.get_texture theme ButtonAttribute.upper_left_hovered ;
        ll = Theme.get_texture theme ButtonAttribute.lower_left_hovered ;
        ur = Theme.get_texture theme ButtonAttribute.upper_right_hovered ;
        lr = Theme.get_texture theme ButtonAttribute.lower_right_hovered ;
        c  = Theme.get_texture theme ButtonAttribute.center_hovered
      }

  val clicked_texture =
    fun theme ->
      {
        l  = Theme.get_texture theme ButtonAttribute.left_clicked ;
        r  = Theme.get_texture theme ButtonAttribute.right_clicked ;
        b  = Theme.get_texture theme ButtonAttribute.bottom_clicked ;
        t  = Theme.get_texture theme ButtonAttribute.top_clicked ;
        ul = Theme.get_texture theme ButtonAttribute.upper_left_clicked ;
        ll = Theme.get_texture theme ButtonAttribute.lower_left_clicked ;
        ur = Theme.get_texture theme ButtonAttribute.upper_right_clicked ;
        lr = Theme.get_texture theme ButtonAttribute.lower_right_clicked ;
        c  = Theme.get_texture theme ButtonAttribute.center_clicked
      }

  method texture theme = 
      match state with
        | Normal -> 
            if hovered
            then hovered_texture theme
            else normal_texture theme
        | Clicked -> clicked_texture theme


            

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



  method draw target themeset =
    let theme = Theme.Set.find themeset theme_id in
    let use_texture = Theme.get_bool theme ButtonAttribute.use_texture in
    let position = Geometry.position geometry in
    let size = Geometry.size geometry in
    
    if use_texture
    then 
      let x,y = position in
      let w,h = size in

      let floats (a,b) = (float a,float b) in

      let textures = self#texture theme in
      let (topw, border_top)     = floats textures.t#get_size in
      let (botw, border_bottom)  = floats textures.b#get_size in
      let (border_left, lefth)   = floats textures.l#get_size in
      let (border_right, righth) = floats textures.r#get_size in
      let (centw,centh) = floats textures.c#get_size in
      let cw,ch = 
        (w -. border_left -. border_right),
        (h -. border_top -. border_bottom)
      in

      let open OcsfmlGraphics in
      let ul_sprite = new sprite 
        ~texture:textures.ul
        ~position () in
      let t_sprite = new sprite
        ~texture:textures.t
        ~position:(x +. border_left, y)
        ~scale:(cw /. topw, 1.) 
        (* ~texture_rect:{left = 0 ; top = 0;width = cw;height = border_top} *) () in
      let ur_sprite = new sprite
        ~texture:textures.ur
        ~position:(x +. w -. border_right, y) () in
      let l_sprite = new sprite
        ~texture:textures.l
        ~position:(x, y +. border_top)
        ~scale:(1., ch /. lefth)
        (* ~texture_rect:{left = 0;top = 0;width = border_left;height = ch} *) () in
      let c_sprite = new sprite
        ~texture:textures.c
        ~position:(x +. border_left, y +. border_top)
        ~scale:(cw /. centw, ch /. centh)
        (* ~texture_rect:{left = 0;top = 0;width = cw;height = ch} *) () in
      let r_sprite = new sprite
        ~texture:textures.r
        ~position:(x +. w -. border_right, y +. border_top)
        ~scale:(1., ch /. righth)
        (* ~texture_rect:{left = 0;top = 0;width = border_right;height = ch} *) () in
      let ll_sprite = new sprite 
        ~texture:textures.ll
        ~position:(x, y +. h -. border_bottom) () in
      let b_sprite = new sprite
        ~texture:textures.b
        ~position:(x +. border_left, y +. h -. border_bottom)
        ~scale:(cw /. botw, 1.)
        (* ~texture_rect:{left = 0;top = 0;width = cw;height = border_bottom} *) () in
      let lr_sprite = new sprite
        ~texture:textures.lr
        ~position:(x +. w -. border_right,y +. h -. border_bottom) ()
      in
      target#draw ul_sprite ;
      target#draw t_sprite ;
      target#draw ur_sprite ;
      target#draw l_sprite ;
      target#draw c_sprite ;
      target#draw r_sprite ;
      target#draw ll_sprite ;
      target#draw b_sprite ;
      target#draw lr_sprite 
    else (
      let fill_color = color state hovered theme in
      let shape = new OcsfmlGraphics.rectangle_shape
        ~position ~size ~fill_color ()
      in
      target#draw shape 
    ) ;
    child#draw target themeset



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
