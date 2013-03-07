module LabelAttribute =
struct
  let label_text_color = Theme.Attribute.Identifier.create ()
end



class label ?(text="") theme =
object(self)
  inherit Widget.widget (Geometry.create (0.,0.) (50.,30.)) as super

  val mutable text = text
  val theme_id = theme
(*  val mutable font = new
  initializer  Gc.finalise (fun lbl -> lbl#font#destroy) self ; *)

  method set_text t =
    text <- t
  method text = text

  method update_geometry area =
    super#update_geometry area

  method draw target themeset =
    let theme = Theme.Set.find themeset theme_id in
    let font = Theme.get_font theme (Theme.GlobalAttribute.font) in
    let character_size = 
      Theme.get_int theme (Theme.GlobalAttribute.character_size) in
    let color = Theme.get_color theme (LabelAttribute.label_text_color) in

    let old_view = new OcsfmlGraphics.view (`Copy target#get_view) in
    let left,top = Geometry.position geometry in
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
      ~color
      ~character_size
      ~font
      ~position () 
    in
    let bounds = text#get_local_bounds in
    let center = OcsfmlGraphics.(bounds.width /. 2., bounds.height /. 2.) in
    text#set_origin_v center ;
    target#set_view view ;
    target#draw text ;
    target#set_view old_view

  method private on_event _ = 
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
