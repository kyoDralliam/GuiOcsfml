class layout swap_xy = 
object(self)
  inherit Widget.widget (Geometry.create (0., 0.) (10.,10.)) as super
  val mutable children : (Widget.widget * float) list = []


  method update_geometry area =
    super#update_geometry area ;
    let (l,t) = swap_xy (Geometry.position area) in
    let (w,h) = swap_xy (Geometry.size area) in
    let total_weight = List.fold_left (+.) 0. (snd (List.split children)) in
    let fold_fun bottom (widget, weight) =
      let height = h *. weight /. total_weight in
      let top = bottom -. height in
      let area = Geometry.create (swap_xy (l,top)) (swap_xy (w, height)) in
      widget#update_geometry area ;
      top 
    in
    ignore (List.fold_left fold_fun (t +. h) children)

  method add : 'a. (#Widget.widget as 'a) -> float -> unit = 
    fun widget weight -> (
      children <- ((widget :> Widget.widget), weight) :: children ;
      self#update_geometry geometry
    )

  method remove (widget: #Widget.widget) =
    children <- List.filter (fun (wid,_) -> wid = widget) children ;
    self#update_geometry geometry


  method draw target =
    let draw ((widget, _) : #Widget.widget * 'a)
      = widget#draw target in
    List.iter draw children

  method handle_event sfev =
    List.exists (fun (wid,_) -> wid#handle_event sfev) children

  method private on_event _ = false

end

class vertical_layout = layout (fun x -> x) 
class horizontal_layout = layout (fun (x,y) -> (y,x))
