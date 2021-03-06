open OcsfmlGraphics

class line_edit = 
object(self)
  inherit Widget.widget (Geometry.create (0.,0.) (100.,20.)) as super

  val mutable text = ""
  val mutable font = new font (`File "Arial.ttf")
  val mutable cursor = 0
  initializer  Gc.finalise (fun lbl -> lbl#font#destroy) self ;

  method font = font

  method update_geometry area =
    super#update_geometry area

  method draw target themeset =
    let old_view = new view (`Copy target#get_view) in
    let left,top = Geometry.position geometry in
    let width,height = Geometry.size geometry in

    
    let outline_thickness = if focused then Some 1. else None in
    let box = new rectangle_shape
      ~fill_color:Color.white
      ~outline_color:Color.blue
      ?outline_thickness
      ~position:(left,top)
      ~size:(width,height) ()
    in
    target#draw box ;


    let view = (
      new view FloatRect.(`Rect { left = 0. ; top = 0.; width ; height })
    ) in
    
    let ratioRect = 
      let wTarget,hTarget = 
        let w,h = target#get_size in
        float_of_int w, float_of_int h
      in
      let ratL, ratT = left /. wTarget, top /. hTarget in
      let ratW, ratH = width /. wTarget, height /. hTarget in
      FloatRect.({ left = ratL; top = ratT; width = ratW; height = ratH })
    in
    view#set_viewport ratioRect ;
    
    let printable_text = new text 
      ~string:text
      ~character_size:10
      ~font:font
      ~color:Color.black () 
    in
    let (lastX,lastY) = printable_text#find_character_pos cursor in
    let position = 
      if lastX > (width -. 1.5)
      then (width -. lastX -. 1.5, height /. 2.)
      else (0., height /. 2.)
    in
    let bounds = printable_text#get_local_bounds in
    let center = (0., bounds.FloatRect.height /. 2.) in
    printable_text#set_position_v position ;
    printable_text#set_origin_v center ;
    target#set_view view ;
    target#draw printable_text ;

    let vertical_bar = 
      let position = 
        if lastX > width -. 1.5
        then (width -. 1.5, 2.)
        else (lastX, 2.) 
      in
      let size = (1., height -. 2.) in
      new rectangle_shape
        ~fill_color:Color.black
        ~size
        ~position ()
    in
    target#draw vertical_bar;
    target#set_view old_view

  method private on_event ev =
    Event.(match ev with
      | TextEntered c -> begin
        if c = 8 (* backspace *) && text <> ""
        then
          let length = String.length text in
          text <-
            if cursor = length
            then String.sub text 0 (length - 1)
            else if cursor <> 0
            then String.sub text 0 (cursor - 1) ^ 
              String.sub text cursor (length - cursor) 
            else text;
          cursor <- max 0 (cursor - 1)
        else if c <> 8 && c < 255
        then 
          let length = String.length text in
          text <- 
            String.sub text 0 cursor ^ 
            String.make 1 (Char.chr c) ^ 
            String.sub text cursor (length - cursor) ;
          cursor <- min (length + 1) (cursor + 1)
      end ;
          true
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
    
