open OcsfmlGraphics

module ButtonAttribute =
struct
  let use_texture = Theme.Attribute.Identifier.create ()

  (* Don't use texture *)
  let color_normal  = Theme.Attribute.Identifier.create ()
  let color_hovered = Theme.Attribute.Identifier.create ()
  let color_clicked = Theme.Attribute.Identifier.create ()

  (* Use texture *)
  let texture_normal = Texture3x3AttributesIdentifier.create ()
  let texture_hovered = Texture3x3AttributesIdentifier.create ()
  let texture_clicked = Texture3x3AttributesIdentifier.create ()

  let setup_textured_button theme ~normal ~hovered ~clicked =
    let theme = Theme.add_bool theme use_texture true in
    let theme = Texture3x3.add_to_theme normal  theme texture_normal  in
    let theme = Texture3x3.add_to_theme hovered theme texture_hovered in
    let theme = Texture3x3.add_to_theme clicked theme texture_clicked in
    theme

  let setup_defaults theme =
    let theme = Theme.add_bool theme
      use_texture false in
    let theme = Theme.add_color theme
      color_normal (Color.rgb 20 50 20) in
    let theme = Theme.add_color theme
      color_hovered (Color.rgb 30 80 30) in
    let theme = Theme.add_color theme
      color_clicked (Color.rgb 5 30 5) in
    theme

end


type state = Normal | Clicked
type button_texture = {
  l: IntRect.t;
  r: IntRect.t;
  b: IntRect.t;
  t: IntRect.t;
  ul: IntRect.t;
  ll: IntRect.t;
  ur: IntRect.t;
  lr: IntRect.t;
  c: IntRect.t
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
            then Theme.get theme ButtonAttribute.color_hovered Theme.Color 
            else Theme.get theme ButtonAttribute.color_normal Theme.Color 
        | Clicked -> Theme.get theme ButtonAttribute.color_clicked Theme.Color

  method texture3x3 theme = 
      match state with
        | Normal -> 
            if hovered
            then Texture3x3.get_from_theme theme ButtonAttribute.texture_hovered
            else Texture3x3.get_from_theme theme ButtonAttribute.texture_normal
        | Clicked -> Texture3x3.get_from_theme theme ButtonAttribute.texture_clicked


            

  method draw target themeset =
    let theme = Theme.Set.find themeset theme_id in
    let use_texture = Theme.get theme ButtonAttribute.use_texture Theme.Bool in
    let position = Geometry.position geometry in
    let size = Geometry.size geometry in
    
    if use_texture
    then 
      let texture = self#texture3x3 theme in
      Texture3x3.draw target texture position size
    else (
      let fill_color = color state hovered theme in
      let shape = new rectangle_shape
        ~position ~size ~fill_color ()
      in
      target#draw shape 
    ) ;
    child#draw target themeset



  method press = state <- Clicked
  method release = state <- Normal ; onClick ()
    
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
