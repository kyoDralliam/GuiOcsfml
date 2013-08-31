open OcsfmlGraphics

type t = {
  texture: texture ;
  upper_left : IntRect.t ;
  lower_left : IntRect.t ;
  upper_right: IntRect.t ;
  lower_right: IntRect.t ;
  left       : IntRect.t ;
  right      : IntRect.t ;
  top        : IntRect.t ; 
  bottom     : IntRect.t ; 
  center     : IntRect.t
} with fields


let create 
    ~upper_left ~lower_left ~upper_right ~lower_right
    ~left ~right ~top ~bottom ~center ~texture () =
  { texture ; upper_left ; lower_left ; upper_right ; lower_right ;
    left ; right ; top ; bottom ; center }


let create_from_subrect ~top_border ~left_border ~right_border ~bottom_border
    rect texture =
  let (x,y) = IntRect.(rect.left,rect.top) in
  let (w,h) = IntRect.(rect.width,rect.height) in
  let t = top_border in
  let l = left_border in
  let r = right_border in
  let b = bottom_border in
  let cw,ch = w - l - r, h - t - b in


  let irect = IntRect.create in
  let upper_left  = irect ~position:(x        , y        ) ~size:(l , t) () in
  let lower_left  = irect ~position:(x        , y + h - b) ~size:(l , b) () in
  let upper_right = irect ~position:(x + w - r, y        ) ~size:(r , t) () in
  let lower_right = irect ~position:(x + w - r, y + h - b) ~size:(r , b) () in
  let top         = irect ~position:(x + l    , y        ) ~size:(cw, t) () in 
  let bottom      = irect ~position:(x + l    , y + h - b) ~size:(cw, b) () in
  let left        = irect ~position:(x        , y + t    ) ~size:(l ,ch) () in
  let right       = irect ~position:(x + w - r, y + t    ) ~size:(r ,ch) () in
  let center      = irect ~position:(x + l    , y + t    ) ~size:(cw,ch) () in

  { texture ; upper_left ; lower_left ; upper_right ; lower_right ;
    left ; right ; top ; bottom ; center }


let add_to_theme texture3x3 theme attributes_id =
  let add_texture id theme texture =
    Theme.add_texture theme (id attributes_id) (Fieldslib.Field.get texture texture3x3) 
  in
  let add_rect id theme rect =
    Theme.add_int_rect theme (id attributes_id) (Fieldslib.Field.get rect texture3x3) 
  in
  let open Fields in
  let open Texture3x3AttributesIdentifier in
  fold     ~init:theme 
        ~texture:(add_texture texture) 
     ~upper_left:(add_rect upper_left)
     ~lower_left:(add_rect lower_left)
    ~upper_right:(add_rect upper_right)
    ~lower_right:(add_rect lower_right)
           ~left:(add_rect left)
          ~right:(add_rect right)
            ~top:(add_rect top)
         ~bottom:(add_rect bottom)
         ~center:(add_rect center)
    
    
let get_from_theme theme attributes_id =
  let get_texture id texture =
    Theme.get theme (id attributes_id) Theme.Texture
  in
  let get_rect id rect =
    Theme.get theme (id attributes_id) Theme.IntRect
  in
  let open Fields in
  let open Texture3x3AttributesIdentifier in
  map 
    ~texture:(get_texture texture)
     ~upper_left:(get_rect upper_left)
     ~lower_left:(get_rect lower_left)
    ~upper_right:(get_rect upper_right)
    ~lower_right:(get_rect lower_right)
           ~left:(get_rect left)
          ~right:(get_rect right)
            ~top:(get_rect top)
         ~bottom:(get_rect bottom)
         ~center:(get_rect center)


let draw (target:#OcsfmlGraphics.render_target) texture ?(draw_center=true) position (w,h)=
  let (x,y) = position in
  let size r = 
    IntRect.(float r.width,float r.height) 
  in


  let (topw, border_top)     = size texture.top in
  let (botw, border_bottom)  = size texture.bottom in
  let (border_left, lefth)   = size texture.left in
  let (border_right, righth) = size texture.right in
  let (centw,centh) = size texture.center in
  let cw,ch = 
    (w -. border_left -. border_right),
    (h -. border_top -. border_bottom)
  in


  let open OcsfmlGraphics in
  let ul_sprite = new sprite 
    ~texture:texture.texture
    ~position
    ~texture_rect:texture.upper_left () in
  let t_sprite = new sprite
    ~texture:texture.texture
    ~position:(x +. border_left, y)
    ~scale:(cw /. topw, 1.) 
    ~texture_rect:texture.top () in
  let ur_sprite = new sprite
    ~texture:texture.texture
    ~position:(x +. w -. border_right, y)
    ~texture_rect:texture.upper_right () in
  let l_sprite = new sprite
    ~texture:texture.texture
    ~position:(x, y +. border_top)
    ~scale:(1., ch /. lefth)
    ~texture_rect:texture.left () in
  let c_sprite = new sprite
    ~texture:texture.texture
    ~position:(x +. border_left, y +. border_top)
    ~scale:(cw /. centw, ch /. centh)
    ~texture_rect:texture.center () in
  let r_sprite = new sprite
    ~texture:texture.texture
    ~position:(x +. w -. border_right, y +. border_top)
        ~scale:(1., ch /. righth)
    ~texture_rect:texture.right () in
  let ll_sprite = new sprite 
    ~texture:texture.texture
    ~position:(x, y +. h -. border_bottom)
    ~texture_rect:texture.lower_left () in
  let b_sprite = new sprite
    ~texture:texture.texture
    ~position:(x +. border_left, y +. h -. border_bottom)
    ~scale:(cw /. botw, 1.)
    ~texture_rect:texture.bottom () in
  let lr_sprite = new sprite
    ~texture:texture.texture
    ~position:(x +. w -. border_right,y +. h -. border_bottom)
    ~texture_rect:texture.lower_right ()
  in
  target#draw ul_sprite ;
  target#draw t_sprite ;
  target#draw ur_sprite ;
  target#draw l_sprite ;

  if draw_center 
  then  target#draw c_sprite ;

  target#draw r_sprite ;
  target#draw ll_sprite ;
  target#draw b_sprite ;
  target#draw lr_sprite 
