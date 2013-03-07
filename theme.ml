module Identifier =
struct
  type t = int
  let compare : t -> t -> int = 
    Pervasives.compare

  let counter = ref (-1)
    
  let create () =
    incr counter ; !counter
end


module Attribute =
struct
  module Identifier =
  struct
    type t = int
    let compare : t -> t -> int =
      Pervasives.compare
      
    let counter = ref (-1)
      
    let create () =
      incr counter ; !counter
  end

  type t =
    | Font of OcsfmlGraphics.font
    | Texture of OcsfmlGraphics.texture
    | Color of OcsfmlGraphics.Color.t
    | Int of int
    | Float of float
end



module Theme = Map.Make(Identifier)
type t = Attribute.t Theme.t

exception Invalid

let create () =
  (ref Theme.empty, Identifier.create ())

let find theme id =
  try Theme.find id theme 
  with Not_found -> raise Invalid

let finalise_font f = f#destroy
let finalise_texture t = t#destroy

let add_font theme id f =
  let f' = new OcsfmlGraphics.font (`Copy f) in
  Gc.finalise finalise_font f' ;
  Theme.add id (Attribute.Font f') theme

let add_texture theme id (image,rect) =
  let t' = new OcsfmlGraphics.texture ~rect (`Image image) in
  t'#set_repeated true ;
  Gc.finalise finalise_texture t' ;
  Theme.add id (Attribute.Texture t') theme

let add_color theme id c =
  Theme.add id (Attribute.Color c) theme

let add_int theme id i =
  Theme.add id (Attribute.Int i) theme

let add_float theme id f =
  Theme.add id (Attribute.Float f) theme


let get_font theme id =
  match find theme id with
    | Attribute.Font f -> f
    | _ -> raise Invalid

let get_texture theme id =
  match find theme id with
    | Attribute.Texture t -> t
    | _ -> raise Invalid

let get_color theme id =
  match find theme id with
    | Attribute.Color c -> c
    | _ -> raise Invalid

let get_int theme id =
  match find theme id with
    | Attribute.Int i -> i
    | _ -> raise Invalid

let get_float theme id =
  match find theme id with
    | Attribute.Float f -> f
    | _ -> raise Invalid

(* class theme ~font () =
object(self)
  val mutable font = 
    match font with
      | Some font -> new OcsfmlGraphics.font (`Copy font)
      | None -> new OcsfmlGraphics.font `None

    
  val mutable top_border_texture    = new OcsfmlGraphics.texture `None
  val mutable bottom_border_texture = new OcsfmlGraphics.texture `None
  val mutable left_border_texture   = new OcsfmlGraphics.texture `None
  val mutable right_border_texture  = new OcsfmlGraphics.texture `None

  val mutable upper_left_corner_texture  = new OcsfmlGraphics.texture `None
  val mutable lower_left_corner_texture  = new OcsfmlGraphics.texture `None
  val mutable upper_right_corner_texture = new OcsfmlGraphics.texture `None
  val mutable lower_right_corner_texture = new OcsfmlGraphics.texture `None

  val mutable top_button_texture    = new OcsfmlGraphics.texture `None
  val mutable bottom_button_texture = new OcsfmlGraphics.texture `None
  val mutable left_button_texture   = new OcsfmlGraphics.texture `None
  val mutable right_button_texture  = new OcsfmlGraphics.texture `None
  val mutable upper_left_button_texture  = new OcsfmlGraphics.texture `None
  val mutable lower_left_button_texture  = new OcsfmlGraphics.texture `None
  val mutable upper_right_button_texture = new OcsfmlGraphics.texture `None
  val mutable lower_right_button_texture = new OcsfmlGraphics.texture `None
  val mutable central_button_texture = new OcsfmlGraphics.texture `None


  initializer Gc.finalize (fun t -> t#font#destroy) self

  method font = font
  method set_font f =
    font#affect f

  method set_border_texture ~top ~bottom ~left ~right 
    ~upper_left ~lower_left ~upper_right ~lower_right () =
    let affect_texture (image, rect) affect = 
end
*)

module Set =
struct
  module ThemeSet = Map.Make(Identifier)
  type u = t ThemeSet.t
  type t = u

  let create () =
    ThemeSet.empty

  let add themeset id theme =
    ThemeSet.add id theme themeset

  let find themeset id =
    ThemeSet.find id themeset
end


module GlobalAttribute =
struct
  let font = Attribute.Identifier.create ()
  let character_size = Attribute.Identifier.create ()
end
