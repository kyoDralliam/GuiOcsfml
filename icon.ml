class icon ?(image=new OcsfmlGraphics.texture `None) () =
object(self)
  inherit Widget.widget (Geometry.create (0.,0.) (50.,50.)) as super
  val image = new OcsfmlGraphics.texture (`Copy image)
  initializer  Gc.finalise (fun lbl -> lbl#image#destroy) self ;

  method image = image

  method draw target =
    let imw,imh = 
      let w,h = image#get_size in
      float w, float h
    in
    let width,height = Geometry.size geometry in
    let left,w = 
      if imw <= width
      then 0.,imw
      else (imw -. width) /. 2.,width
    in
    let top,h =
      if imh <= height
      then 0.,imh
      else (imh -. height) /. 2.,height
    in

    let texture_rect =
      OcsfmlGraphics.({ left = truncate left; 
                        top = truncate top; 
                        width = truncate w ; 
                        height = truncate h })
    in
    let origin = (w /. 2., h /. 2.) in
    let position = (left +. width /. 2., top +. height /. 2.) in
    let sprite = new OcsfmlGraphics.sprite
      ~texture:image
      ~origin
      ~position
      ~texture_rect ()
    in target#draw sprite

  method on_event _ = false
      
end
