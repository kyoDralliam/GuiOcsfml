
type t = {
  texture    : Theme.Attribute.Identifier.t ;
  upper_left : Theme.Attribute.Identifier.t ;
  lower_left : Theme.Attribute.Identifier.t ;
  upper_right: Theme.Attribute.Identifier.t ;
  lower_right: Theme.Attribute.Identifier.t ;
  left       : Theme.Attribute.Identifier.t ;
  right      : Theme.Attribute.Identifier.t ;
  top        : Theme.Attribute.Identifier.t ; 
  bottom     : Theme.Attribute.Identifier.t ; 
  center     : Theme.Attribute.Identifier.t 
} with fields

let create () =
  let id () = Theme.Attribute.Identifier.create () in
  Fields.create 
    ~texture:(id ()) 
    ~upper_left:(id ()) ~lower_left:(id ())
    ~upper_right:(id ()) ~lower_right:(id ())
    ~top:(id ()) ~bottom:(id ())
    ~left:(id ()) ~right:(id ())
    ~center:(id ())
