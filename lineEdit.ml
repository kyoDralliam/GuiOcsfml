class line_edit = 
object(self)
  inherit Widget.widget (Geometry.create (0.,0.) (100.,20.)) as super

  val mutable text = ""
  val mutable font = new OcsfmlGraphics.font (`File "Arial.ttf") 
  initializer  Gc.finalise (fun lbl -> lbl#font#destroy) self ;

  method font = font

  method update_geometry area =
    super#update_geometry area

  method draw target =
    let old_view = new OcsfmlGraphics.view (`Copy target#get_view) in
    let left,top = Geometry.position geometry in
    let width,height = Geometry.size geometry in

    let box = new OcsfmlGraphics.rectangle_shape
      ~fill_color:OcsfmlGraphics.Color.white
      ~position:(left,top)
      ~size:(width,height) ()
    in
    target#draw box ;


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
    
    let position = (0., height /. 2.) in
    let text = new OcsfmlGraphics.text 
      ~string:text
      ~character_size:10
      ~font:font
      ~color:OcsfmlGraphics.Color.black
      ~position () 
    in
    let bounds = text#get_local_bounds in
    let center = OcsfmlGraphics.(0., bounds.height /. 2.) in
    text#set_origin_v center ;
    target#set_view view ;
    target#draw text ;
    target#set_view old_view

  method private on_event ev =
    Event.(match ev with
      | TextEntered c -> (
        if c = 8 (* backspace *) && text <> ""
        then text <- String.(sub text 0 (length text - 1))
        else if c <> 8
        then text <- text ^ Char.(escaped (chr c)) ;
        true
      )
      | _ -> false
    )
end
    
