class vertical_layout = 
object(self)
  inherit Widget.widget (Geometry.create (0., 0.) (10.,10.)) as super
  val mutable children : (Widget.widget * float) list = []


  method update_geometry area =
    super#update_geometry area ;
    let (l,t) = Geometry.position area in
    let (w,h) = Geometry.size area in
    let totalw = List.fold_left (fun part (_, w) -> part +. w) 0. children in
    ignore ( List.fold_left (fun bot (wid, weight) ->
      let height = h *. weight /. totalw in
      let top = bot -. height in
      let area = Geometry.create (l,top) (w, height) in
      wid#update_geometry area ;
      top 
    ) (t +. h) children)

  method add : 'a. (#Widget.widget as 'a) -> float -> unit = 
    fun widget weight -> (
      children <- ((widget :> Widget.widget), weight) :: children ;
      self#update_geometry geometry
    )

  method remove (widget: #Widget.widget) =
    children <- List.filter (fun (wid,_) -> wid = widget) children ;
    self#update_geometry geometry


  method draw target =
    let draw ((widget, _) : #Widget.widget * float)
      = widget#draw target in

    List.iter draw children

  method onEvent ev =
    List.exists (fun (wid,_) -> wid#onEvent ev) children

end
