
class label ?(text="") () =
object(self)
  inherit Widget.widget (Geometry.create (0.,0.) (50.,30.)) as super

  val mutable text = text
  val mutable font = new OcsfmlGraphics.font (`File "Arial.ttf") 
  initializer  Gc.finalise (fun lbl -> lbl#font#destroy) self ;

  method set_text t =
    text <- t
  method text = text

  method set_font f =
    font#destroy ;
    font <- f
  method font = font

  method update_geometry area =
    super#update_geometry area

  method draw target =
    let old_view = new OcsfmlGraphics.view (`Copy target#get_view) in
    let (left,top) = Geometry.position geometry in
    let width,height = Geometry.size geometry in

    let view = OcsfmlGraphics.(
      new view (`Rect { left = 0. ; top = 0.; width ; height })
    ) in
    
    let ratioRect = 
      let wTarget,hTarget = 
        let w,h = target#get_size in
        float_of_int w, float_of_int h
      in
      let ratL, ratT = left /. wTarget, top /. hTarget in
      let ratW, ratH = width /. wTarget, height /. hTarget in
      OcsfmlGraphics.({ left = ratL; top = ratT; width = ratW; height = ratH })
    in
    view#set_viewport ratioRect ;
    
    let position = (width /. 2., height /. 2.) in
    let text = new OcsfmlGraphics.text 
      ~string:text
      ~character_size:10
      ~font:font
      ~position () 
    in
    let bounds = text#get_local_bounds in
    let center = OcsfmlGraphics.(bounds.width /. 2., bounds.height /. 2.) in
    text#set_origin_v center ;
    target#set_view view ;
    target#draw text ;
    target#set_view old_view

  method onEvent _ = 
    false
      
end
   
(*let move lbl pos =
  Geometry.move lbl.geometry pos*)

(*let geometry lbl =
  Geometry.copy lbl.geometry *)
    

    

(*
let create ?(text="") () =
  let geometry = Geometry.create (0.,0.) (50.,30.) in
  let font  =  in
  let lbl = { geometry ; text ; font } in
 
  lbl
    
*)
