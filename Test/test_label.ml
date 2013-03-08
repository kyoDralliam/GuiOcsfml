open OcsfmlWindow
open OcsfmlGraphics


let _ = 
  let win = new render_window (VideoMode.create ()) "Label" in
  win#set_framerate_limit 60 ;

  let my_themeset = ref (Theme.Set.create ()) in
  
  let (my_theme, my_theme_id) = Theme.create () in

  let font = new OcsfmlGraphics.font (`File "Arial.ttf") in
  let button_tex = new OcsfmlGraphics.image (`File "rounded_button.png") in
  button_tex#create_mask_from_color (OcsfmlGraphics.Color.rgb 255 0 255) ;

  
  my_theme := Theme.add_font !my_theme Theme.GlobalAttribute.font font ;
  my_theme := Theme.add_int !my_theme Theme.GlobalAttribute.character_size 10 ;
  my_theme := Theme.add_color 
    !my_theme Label.LabelAttribute.text_color (OcsfmlGraphics.Color.black);


  my_theme := Theme.add_bool !my_theme Button.ButtonAttribute.use_texture true ;

  my_theme := begin
    let normal = Button.ButtonAttribute.({
      upper_left  = ( 24,34, 11,10) ; lower_left  = ( 24,48, 11,10) ;
      upper_right = (163,34, 11,10) ; lower_right = (163,48, 11,10) ;
      top         = ( 35,34,128,10) ; bottom      = ( 35,48,128,10) ;
      left        = ( 24,44, 11, 4) ; right       = (163,44, 11, 4) ;
      center      = ( 35,44,128, 4) 
    }) in
    let hovered = normal in
    let clicked = Button.ButtonAttribute.({
      upper_left  = (184,34, 11,10) ; lower_left  = (184,48, 11,10) ;
      upper_right = (323,34, 11,10) ; lower_right = (323,48, 11,10) ;
      top         = (195,34,128,10) ; bottom      = (195,48,128,10) ;
      left        = (184,44, 11, 4) ; right       = (323,44, 11, 4) ;
      center      = (195,44,128, 4) 
    }) in
    Button.ButtonAttribute.setup_textured_button !my_theme 
      ~normal ~hovered ~clicked button_tex 
  end ;
    
  font#destroy ;
  button_tex#destroy ;



  my_themeset := Theme.Set.add !my_themeset my_theme_id !my_theme ;
  


  let label1 = new Label.label ~text:"Hello Gui Ocsfml" my_theme_id in
  let button1 = new Button.button 
    ~onClick:(fun () -> print_string "Hello\n" ; flush stdout) my_theme_id
    (label1 :> Widget.widget) 
  in

  let label2 = new Label.label ~text:"Goodbye Gui Ocsfml" my_theme_id in
  let button2 = new Button.button ~onClick:(fun () -> win#close) my_theme_id (label2 :> Widget.widget) in


  let label3 = new Label.label ~text:"Do nothing" my_theme_id in
  let button3 = new Button.button my_theme_id (label3 :> Widget.widget) in

  let label4 = new Label.label ~text:"Je suis un contestataire" my_theme_id in
  let button4 = new Button.button my_theme_id (label4 :> Widget.widget) in 


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
    
    if win#is_open
    then (
      event_loop win ;
      win#clear () ;
      widget#draw win !my_themeset ;
      win#display ;
      loop ()
    )
  in loop ()
