type layout_item = {
  widget : Widget.widget ;
  min : float ;
  max : float ;
  weight : float ;
}

let rec resize xi d = function
  | 0 -> xi 
  | k -> 
      let x0 = List.fold_left (fun x (_,y,_) -> x+.y) 0. xi in
      let f (min_x, x, max_x) = (min_x, min max_x (max min_x (x*.d/.x0)), max_x) in
      resize (List.map f xi) d (k-1)

let init_resize l d = 
  let alpha = List.fold_left (fun x y -> x +. y.weight)  0. l in 
  let xi_0 = List.map (fun x -> (x.min, x.weight *. d /. alpha, x.max)) l in 
  let k = 3 in
  List.map (fun (_,x,_) -> x) (resize xi_0 d k)


class layout swap_xy = 
object(self)
  inherit Widget.widget (Geometry.create (0., 0.) (10.,10.)) as super
  val mutable children : layout_item list = []


  method update_geometry area =
    super#update_geometry area ;
    let (l,t) = swap_xy (Geometry.position area) in
    let (w,h) = swap_xy (Geometry.size area) in
    let xi = init_resize children h in
    let fold_fun bottom case height =
      let top = bottom -. height in
      let area = Geometry.create (swap_xy (l,top)) (swap_xy (w, height)) in
      case.widget#update_geometry area ;
      top 
    in
    ignore (List.fold_left2 fold_fun (t +. h) children xi)

  method add : 'a. (#Widget.widget as 'a) -> ?min:float -> ?max:float -> float -> unit = 
    fun widget ?(min=0.) ?(max=infinity) weight -> (
      children <- { widget = (widget :> Widget.widget) ; min ; max ; weight} :: children ;
      self#update_geometry geometry
    )

  method remove (widget0 : #Widget.widget) =
    children <- List.filter (fun { widget ; _ } -> widget = widget0) children ;
    self#update_geometry geometry


  method draw target =
    List.iter (fun { widget ; _ } -> widget#draw target) children

  method handle_event sfev =
    List.exists (fun { widget ; _ } -> widget#handle_event sfev) children

  method private on_event _ = false

end

class vertical_layout = layout (fun x -> x) 
class horizontal_layout = layout (fun (x,y) -> (y,x))

