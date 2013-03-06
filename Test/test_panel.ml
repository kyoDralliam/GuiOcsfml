open OcsfmlWindow
open OcsfmlGraphics

let _ = 
  let app = new render_window (VideoMode.create ()) "Label" in
  app#set_framerate_limit 60 ;

  let label1 = new Label.label ~text:"Hello Gui Ocsfml" () in
  let button1 = new Button.button 
    ~onClick:(fun () -> print_string "Hello\n" ; flush stdout)
    (label1 :> Widget.widget) 
  in

  let label2 = new Label.label ~text:"Goodbye Gui Ocsfml" () in
  let button2 = new Button.button ~onClick:(fun () -> app#close) (label2 :> Widget.widget) in


  let label3 = new Label.label ~text:"Do nothing" () in
  let button3 = new Button.button (label3 :> Widget.widget) in

  let label4 = new Label.label ~text:"Je suis un contestataire" () in
  let button4 = new Button.button (label4 :> Widget.widget) in 

  let left_layout = new Layout.vertical_layout in
  left_layout#add (Widget.empty_widget) 1.  ;
  left_layout#add button2            5.   ;
  left_layout#add (Widget.empty_widget) 0.1 ;
  left_layout#add button1            5.   ;
  left_layout#add (Widget.empty_widget) 0.1 ;
  left_layout#add button3            5.   ;
  left_layout#add (Widget.empty_widget) 1.  ;

  let right_layout = new Layout.vertical_layout in 
  right_layout#add (Widget.empty_widget) 1.  ;
  right_layout#add button4            15.2 ;
  right_layout#add (Widget.empty_widget) 1.  ;
  
  let hlayout = new Layout.horizontal_layout in 
  hlayout#add (Widget.empty_widget) 1.   ;
  hlayout#add left_layout        5.   ;
  hlayout#add (Widget.empty_widget) 0.1 ;
  hlayout#add right_layout       5.   ;
  hlayout#add (Widget.empty_widget) 1.   ;

  
  let panel1 = new Panel.panel 
    ~geometry:(Geometry.create (100., 100.) (400., 130.)) 
    ~dragable:true
    ~child:(hlayout :> Widget.widget)
    () 
  in
  
  let panel2 = new Panel.panel
    ~geometry:(Geometry.create (10., 10.) (50., 50.))
    ~background_color:(OcsfmlGraphics.Color.rgba 255 0 0 150)
    ~dragable:true
    ()
  in

  let panels = ref [panel1 ; panel2] in

  let rec loop () =
    let handle_event e =
      let open Event in 
          match e with
            | Resized { width ; height } ->
                let width,height = (float width, float height) in
                let vrect = OcsfmlGraphics.({ left = 0.; top = 0.; width; height}) in
                let v = new OcsfmlGraphics.view (`Rect vrect) in
                app#set_view v 
            | Closed | KeyPressed { code = KeyCode.Escape ; _ } -> app#close
            | _ -> 
                let fold_fun (ev_opt, acc) widget =
                  match ev_opt with 
                    | None -> (None, widget::acc) 
                    | Some e -> 
                        if widget#handle_event e 
                        then (None, acc @ [widget]) 
                        else (Some e, widget::acc)
                in 
                panels := List.rev (snd (List.fold_left fold_fun (Some e, []) !panels))
      in
      
      let rec event_loop () =
        match app#poll_event with
          | Some e -> (handle_event e ; event_loop ())
          | None -> ()
      in
      
      if app#is_open
      then begin
        event_loop () ;
        app#clear () ;
        List.iter (fun (widget: #Widget.widget) -> widget#draw app) (List.rev !panels) ;
        app#display ;
        loop ()
      end
    in loop () 
