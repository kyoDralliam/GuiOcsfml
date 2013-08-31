open OcsfmlGraphics

module FrameAttribute =
struct
  let use_texture = Theme.Attribute.Identifier.create ()
  let color = Theme.Attribute.Identifier.create ()
  let texture = Texture3x3AttributesIdentifier.create ()   
end

class frame ?(child=Widget.empty_widget) ?(margins=5.) theme_id =
object
  inherit Widget.widget (Geometry.create (0.,0.) (10., 10.)) as super

  val child = child
  val margins : float = margins
  val theme_id = theme_id
                           
  method draw : 
    'a . (#render_target as 'a) -> Theme.Set.t -> unit =
    fun target themeset ->
      let theme = Theme.Set.find themeset theme_id in
      let use_texture = Theme.get theme FrameAttribute.use_texture Theme.Bool in

      let position = Geometry.position geometry in
      let size = Geometry.size geometry in
        
      
      if use_texture
      then
        let texture = Texture3x3.get_from_theme theme FrameAttribute.texture in
	Texture3x3.draw target texture ~draw_center:false position size 
      else begin
        let outline_color = Theme.get theme FrameAttribute.color Theme.Color in
        let shape = new rectangle_shape
          ~outline_thickness:(2. *. margins)
          ~outline_color
          ~fill_color:(Color.rgba 0 0 0 0)
          ~position
          ~size ()
        in
        target#draw shape 
      end ;
      child#draw target themeset
        

  method update_geometry area = 
    super#update_geometry area ;
    
    let (x,y) = Geometry.position area in
    let (w,h) = Geometry.size area in
    let area = Geometry.create 
      (x +.       margins, y +.       margins)
      (w -. 2. *. margins, h -. 2. *. margins)
    in
    child#update_geometry area 

  method handle_event sfev = 
    child#handle_event sfev || super#handle_event sfev
      
  method private on_event event = false
end
