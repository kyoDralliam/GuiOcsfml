class line_edit = 
object(self)
  inherit Widget.widget (Geometry.create (0.,0.) (100.,20.)) as super

  val mutable text = ""
  val mutable font = new OcsfmlGraphics.font (`File "Arial.ttf")
  val mutable cursor = 0
  initializer  Gc.finalise (fun lbl -> lbl#font#destroy) self ;

  method font = font

  method update_geometry area =
    super#update_geometry area

  method draw target =
    let old_view = new OcsfmlGraphics.view (`Copy target#get_view) in
    let left,top = Geometry.position geometry in
    let width,height = Geometry.size geometry in

    
    let outline_thickness = if focused then Some 1. else None in
    let box = new OcsfmlGraphics.rectangle_shape
      ~fill_color:OcsfmlGraphics.Color.white
      ~outline_color:OcsfmlGraphics.Color.blue
      ?outline_thickness
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

    let vertical_bar = 
      let position = text#find_character_pos cursor in
      let size = (1., height -. 1.) in
      new OcsfmlGraphics.rectangle_shape
        ~fill_color:OcsfmlGraphics.Color.black
        ~size
        ~position ()
    in
    target#draw vertical_bar;
    target#set_view old_view

  method private on_event ev =
    Event.(match ev with
      | TextEntered c -> (
        if c = 8 (* backspace *) && text <> ""
        then
          let length = String.length text in
          text <-
            if cursor = length
            then String.sub text 0 (length - 1)
            else if cursor <> 0
            else String.sub text 0 (cursor - 1) ^ 
              String.sub text cursor (length - cursor) ;
          cursor <- max 0 (cursor - 1)
        else if c <> 8
        then begin
          text <- text ^ String.make 1 (Char.chr c) ;
          cursor <- cursor + 1
        end ;
        true
      )
      | Pressed ->
          Widget.Focus.grab_focus (self :> Widget.widget)
      | KeyPressed code ->
          if code = OcsfmlWindow.KeyCode.Left
          then (cursor <- max 0 (cursor-1); true)
          else if code = OcsfmlWindow.KeyCode.Right
          then (cursor <- min (String.length text) (cursor+1); true)
          else false
      | _ -> false
    )
end
    
