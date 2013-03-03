type draw_cache = OcsfmlGraphics.render_texture option
    
    
type t = 
    { 
      mutable geometry: Geometry.t ; 
      mutable text: string ; 
      font: OcsfmlGraphics.font 
    }
      



          
          
let set_text lbl t =
  lbl.text <- t 

let move lbl pos =
  Geometry.move lbl.geometry pos

let update_geometry lbl area =
  lbl.geometry <- area

let geometry lbl =
  Geometry.copy lbl.geometry
    

let draw lbl (target:#OcsfmlGraphics.render_target) =
  let (left,top) as position = Geometry.position lbl.geometry in
  let width,height = Geometry.size lbl.geometry in

  let view = OcsfmlGraphics.(
    new view (`Rect { left = 0. ; top = 0.; width ; height })) in

  let ratioRect = 
    let wTarget,hTarget = 
      let w,h = target#get_size in
      float_of_int w, float_of_int h
    in
    let ratL, ratT = left /. wTarget, top /. hTarget in
    let ratW, ratH = width /. wTarget, height /. hTarget in
    OcsfmlGraphics.({ left = ratL ; top = ratT ; width = ratW ; height = ratH })
  in
  view#set_viewport ratioRect ;

  let text = new OcsfmlGraphics.text 
    ~string:lbl.text
    ~character_size:10
    ~font:lbl.font
    ~position () 
  in
  target#set_view view ;
  target#draw text
    
let onEvent lbl ev = 
  false


let create ?(text="") () =
  let geometry = Geometry.create (0.,0.) (50.,30.) in
  let font  = new OcsfmlGraphics.font (`File "Arial.ttf") in
  let lbl = { geometry ; text ; font } in
  Gc.finalise (fun lbl -> lbl.font#destroy) lbl ;
  lbl
    
