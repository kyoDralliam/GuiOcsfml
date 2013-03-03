type draw_cache = OcsfmlGraphics.render_texture option


type t = 
    { geometry: Geometry.t ; mutable text: string ; 
      font: OcsfmlGraphics.font ; mutable draw_cache: draw_cache}

let invalid_draw_cache lbl =
  lbl.draw_cache <- None

let get_text_texture lbl =
  match lbl.draw_cache with
    | Some rdr_text -> rdr_text#get_texture
    | None ->
        let target = new OcsfmlGraphics.render_texture 
          (int_of_float lbl.geometry.width)
          (int_of_float lbl.geometry.height) ;
        let text = new OcsfmlGraphics.text 
          ~string:lbl.text
          ~font:lbl.font ()
        in
        target#clear () ;
        target#draw text ;
        target#display ;
        lbl.draw_cache <- target ;
        target#get_texture


let set_text lbl t =
  lbl.text <- t ;
  invalid_draw_cache lbl

let move lbl pos =
  Geometry.mov lbl pos

let resize lbl sz =
  Geometry.resize lbl.geometry sz ;
  invalid_draw_cache lbl
    

let draw lbl target =
  let texture = (get_text_texture lbl) in
  let position = (Geometry.position lbl.geometry) in
  let sprite = new OcsfmlGraphics.sprite 
    ~texture
    ~position () 
  in
  target#draw sprite
      
let create ?(text="") () =
  let geometry = Geometry.create (0.,0.) (50.,30.) in
  let font  = new OcsfmlGraphics.font (`File "arial.ttf") in
  let lbl = { geometry ; text ; font ; None } in
  Gc.finalise (fun lbl -> lbl.font#destroy) lbl ;
  lbl
    
