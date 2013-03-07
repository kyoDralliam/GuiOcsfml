open OcsfmlWindow
open OcsfmlGraphics


let _ = 
  let win1 = new render_window (VideoMode.create ()) "Label" in
  let win2 = new render_window (VideoMode.create ()) "Label" in
  win1#set_framerate_limit 60 ;
  win2#set_framerate_limit 60 ;

  let my_themeset = ref (Theme.Set.create ()) in
  
  let (my_theme, my_theme_id) = Theme.create () in

  let font = new OcsfmlGraphics.font (`File "Arial.ttf") in
  
  my_theme := Theme.add_font !my_theme Theme.GlobalAttribute.font font ;
  my_theme := Theme.add_int !my_theme Theme.GlobalAttribute.character_size 10 ;
  my_theme := Theme.add_color 
    !my_theme Label.LabelAttribute.label_text_color (OcsfmlGraphics.Color.white);

  font#destroy ;

  my_themeset := Theme.Set.add !my_themeset my_theme_id !my_theme ;
  


  let label1 = new Label.label ~text:"Hello Gui Ocsfml" my_theme_id in
  let button1 = new Button.button 
    ~onClick:(fun () -> print_string "Hello\n" ; flush stdout)
    (label1 :> Widget.widget) 
  in

  let label2 = new Label.label ~text:"Goodbye Gui Ocsfml" my_theme_id in
  let button2 = new Button.button ~onClick:(fun () -> win1#close) (label2 :> Widget.widget) in


  let label3 = new Label.label ~text:"Do nothing" my_theme_id in
  let button3 = new Button.button (label3 :> Widget.widget) in

  let label4 = new Label.label ~text:"Je suis un contestataire" my_theme_id in
  let button4 = new Button.button (label4 :> Widget.widget) in 


  let line_edit = new LineEdit.line_edit in

  let left_layout = new Layout.vertical_layout in
  left_layout#add (Widget.empty_widget) 15.  ;
  left_layout#add button2            5.   ;
  left_layout#add (Widget.empty_widget) 0.25 ;
  left_layout#add button1            5.   ;
  left_layout#add (Widget.empty_widget) 0.25 ;
  left_layout#add button3            5.   ;
  left_layout#add (Widget.empty_widget) 15.  ;

  let right_layout = new Layout.vertical_layout in 
  right_layout#add (Widget.empty_widget) 15.  ;
  right_layout#add button4            14. ;
  right_layout#add (Widget.empty_widget) 0.5  ;
  right_layout#add line_edit          1. ;
  right_layout#add (Widget.empty_widget) 15.  ;
  
  let hlayout = new Layout.horizontal_layout in 
  hlayout#add (Widget.empty_widget) 1.   ;
  hlayout#add left_layout        1.   ;
  hlayout#add (Widget.empty_widget) 0.03 ;
  hlayout#add right_layout       1.   ;
  hlayout#add (Widget.empty_widget) 1.   ;
  

  let widget = hlayout in



  let rec loop () =
    let handle_event (win:#OcsfmlGraphics.render_window) e =
      Event.(match e with
        | Resized { width ; height } ->
            let width,height = (float width, float height) in
            let vrect = OcsfmlGraphics.({ left = 0.; top = 0.; width; height}) in
            let v = new OcsfmlGraphics.view (`Rect vrect) in
            win#set_view v ;
            widget#resize (width, height)
        | Closed | KeyPressed { code = KeyCode.Escape ; _ } -> win#close
        | _ -> ignore (widget#handle_event e)
       )
    in

    let rec event_loop (win:#OcsfmlGraphics.render_window) =
      match win#poll_event with
        | Some e -> (handle_event win e ; event_loop win)
        | None -> ()
    in
    
    if win1#is_open && win2#is_open
    then (
      event_loop win1 ;
      event_loop win2 ;
      win1#clear () ;
      win2#clear () ;
      widget#draw win1 !my_themeset ;
      widget#draw win2 !my_themeset ;
      win1#display ;
      win2#display ;
      loop ()
    )
  in loop ()
