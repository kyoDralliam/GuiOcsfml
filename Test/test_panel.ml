open OcsfmlWindow
open OcsfmlGraphics


let _ = 
  let win = new render_window (VideoMode.create ()) "Label" in
  win#set_framerate_limit 60 ;

  let panel1 = new Panel.panel 
    ~geometry:(Geometry.create (100., 100.) (400., 130.)) 
    ~dragable:true
    () in

  let panels = [panel1] in

  let rec loop () =
    let handle_event e =
      let open Event in 
          match e with
            | Resized { width ; height } ->
                let width,height = (float width, float height) in
                let vrect = OcsfmlGraphics.({ left = 0.; top = 0.; width; height}) in
                let v = new OcsfmlGraphics.view (`Rect vrect) in
                win#set_view v 
            | Closed | KeyPressed { code = KeyCode.Escape ; _ } -> win#close
            | _ -> 
                let fold_fun ev_opt widget =
                  match ev_opt with 
                    | None -> None 
                    | Some e -> if widget#handle_event e then None else Some e
                in 
                ignore (List.fold_left fold_fun (Some e) panels)
      in
      
      let rec event_loop () =
        match win#poll_event with
          | Some e -> (handle_event e ; event_loop ())
          | None -> ()
      in
      
      if win#is_open
      then (
        event_loop () ;
        win#clear () ;
        List.iter (fun (widget: #Widget.widget) -> widget#draw win) panels ;
        win#display ;
        loop ()
      )
    in loop () 
