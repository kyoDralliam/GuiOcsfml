  


class virtual extern_widget = object
  method draw : 'a . (#OcsfmlGraphics.render_target as 'a) -> unit
  method resize : float -> float -> unit
end
  
type widget = 
  | Label of Label.t
  | Button of Button.t * widget
  | Extern of extern_widget
      

let rec draw widget target = 
  match widget with
    | Label lab -> Label.draw lab target
    | Button (b, child) -> Button.draw b target draw child
    | Extern e -> e#draw target


let rec resize w h = function 
  | Button (b, child) -> Button.resize b w h resize child 
  | Extern e -> e#resize w h
      




