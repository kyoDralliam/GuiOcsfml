type state = Normal | Hovered | Clicked

type t = {
  mutable geometry: Geometry.t ;
  mutable state: state ;
  mutable onClick : unit -> unit
}


let color btt =
  match btt.state with
    | Normal -> OcsfmlGraphics.Color.rgb 20 50 20
    | Hovered -> OcsfmlGraphics.Color.rgb 30 80 30
    | Clicked -> OcsfmlGraphics.Color.rgb 5 30 5


(*let get_child_texture btt render_func child =
  match btt.draw_cache with
    | Some rdr_texture -> rdr_texture#get_texture
    | None -> 
        let (w,h) = Geometry.size btt.geometry in
        let target = 
          let wi,hi = int_of_float w, int_of_float h in
          new OcsfmlGraphics.render_texture wi hi 
        in
        target#clear ~color:(color btt) () ;
        render_func child target ;
        target#display ;        
        btt.draw_cache <- Some target ;
        target#get_texture *)



let draw btt (target:#OcsfmlGraphics.render_target) render_func child =
  let fill_color = color btt in
  let position = Geometry.position btt.geometry in
  let size = Geometry.size btt.geometry in
  let shape = new OcsfmlGraphics.rectangle_shape
    ~position ~size ~fill_color ()
  in
  target#draw shape ;
  render_func child target

(*  let texture = get_child_texture btt render_func child in
    let sprite = new OcsfmlGraphics.sprite ~texture ~position () in
    let clipping_shape = new OcsfmlGraphics.rectangle_shape
    ~position ~size 
    ~fill_color:(OcsfmlGraphics.Color.rgba 255 255 255 255) ()
    in
    target#draw clipping_shape ;
    target#draw ~blend_mode:OcsfmlGraphics.BlendMultiply sprite
*)

let update_geometry btt area update_geometry_func child =
  btt.geometry <- area ;
  let x,y = Geometry.position area in
  let w,h = Geometry.size area in
  update_geometry_func child 
    (Geometry.create (x +. 5.,y +. 5.) (w -. 10., h -. 10.))

let geometry btt =
  Geometry.copy btt.geometry

let onEvent btt = function
  | Event.Click -> (btt.onClick () ; true)
  | _ -> false


let create ?(onClick=(fun _ -> ())) () =
  let geometry = Geometry.create (0.,0.) (50.,30.) in
  let btt = { geometry ; state = Normal ; onClick } in
  btt

