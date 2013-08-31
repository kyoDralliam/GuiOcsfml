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
    | IntRect of OcsfmlGraphics.IntRect.t
    | FloatRect of OcsfmlGraphics.FloatRect.t
    | Int of int
    | Float of float
    | Bool of bool
end



module Theme = Map.Make(Identifier)
type t = Attribute.t Theme.t
type _ attribute_kind = 
  | Font : OcsfmlGraphics.font attribute_kind
  | Texture : OcsfmlGraphics.texture attribute_kind
  | Color : OcsfmlGraphics.Color.t attribute_kind
  | IntRect : OcsfmlGraphics.IntRect.t attribute_kind
  | FloatRect : OcsfmlGraphics.FloatRect.t attribute_kind
  | Int : int attribute_kind
  | Float : float attribute_kind
  | Bool : bool attribute_kind


exception Invalid

let create () = Theme.empty

let find theme id =
  try Theme.find id theme 
  with Not_found -> raise Invalid

let add_font theme id f =
  Theme.add id (Attribute.Font f) theme

let add_texture theme id t =
  Theme.add id (Attribute.Texture t) theme

let add_color theme id c =
  Theme.add id (Attribute.Color c) theme

let add_int_rect theme id r =
  Theme.add id (Attribute.IntRect r) theme

let add_float_rect theme id r =
  Theme.add id (Attribute.FloatRect r) theme

let add_int theme id i =
  Theme.add id (Attribute.Int i) theme

let add_float theme id f =
  Theme.add id (Attribute.Float f) theme

let add_bool theme id b =
  Theme.add id (Attribute.Bool b) theme

let get : type a. t -> Identifier.t -> a attribute_kind -> a =
              fun theme id kind ->
                match (kind, find theme id) with
                  | (Font     , Attribute.Font      f) -> f
                  | (Texture  , Attribute.Texture   t) -> t
                  | (Color    , Attribute.Color     c) -> c
                  | (IntRect  , Attribute.IntRect   r) -> r
                  | (FloatRect, Attribute.FloatRect r) -> r
                  | (Int      , Attribute.Int       i) -> i
                  | (Float    , Attribute.Float     f) -> f
                  | (Bool     , Attribute.Bool      b) -> b
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
