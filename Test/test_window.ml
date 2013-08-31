open OcsfmlWindow
open OcsfmlGraphics



let font = new OcsfmlGraphics.font (`File "OldeEnglish.ttf")
let yellow_theme_texture = new OcsfmlGraphics.texture (`File "yellow_theme.png") 
let grey_theme_texture = new OcsfmlGraphics.texture (`File "grey_theme.png") 

let general_theme_id = Theme.Identifier.create ()
let window_frame_theme_id = Theme.Identifier.create ()
 

let setup_yellow_themeset () =
  let setup_label theme =
    let theme = Theme.add_font theme Theme.GlobalAttribute.font font in
    let theme = Theme.add_int theme Theme.GlobalAttribute.character_size 30 in
    let theme = Theme.add_color 
        theme Label.LabelAttribute.text_color (OcsfmlGraphics.Color.black) in
    theme
  in
  let setup_button theme =     
    let normal = Texture3x3.create_from_subrect 
      ~left_border:25 ~right_border:25 ~bottom_border:14 ~top_border:14 
      IntRect.({ left = 201 ; top = 114 ; width = 165 ; height = 48}) yellow_theme_texture
    in
    let hovered = Texture3x3.create_from_subrect
      ~left_border:25 ~right_border:25 ~bottom_border:14 ~top_border:14 
      IntRect.({ left = 201 ; top = 178 ; width = 165 ; height = 48}) yellow_theme_texture
    in
    let clicked = Texture3x3.create_from_subrect
      ~left_border:25 ~right_border:25 ~bottom_border:14 ~top_border:14 
      IntRect.({ left = 201 ; top = 242 ; width = 165 ; height = 48}) yellow_theme_texture
    in
    Button.ButtonAttribute.setup_textured_button theme 
      ~normal ~hovered ~clicked 
  in
  let setup_panel theme =
    Theme.add_color theme Panel.PanelAttribute.background_color
      (OcsfmlGraphics.Color.rgb 190 150 50)
  in
  let setup_frame theme =
    let open Frame.FrameAttribute in
    let theme = Theme.add_bool theme use_texture true in

    let texture3x3 = Texture3x3.create_from_subrect 
      ~top_border:8 ~left_border:8 ~right_border:8 ~bottom_border:8
      IntRect.({ left = 82 ; top = 81 ; width = 304 ; height = 304 }) yellow_theme_texture
    in
    Texture3x3.add_to_theme texture3x3 theme texture
  in
  let setup_window_frame theme =
    let open Frame.FrameAttribute in
    let theme = Theme.add_bool theme use_texture true in
    let texture3x3 = Texture3x3.create_from_subrect 
      ~top_border:32 ~left_border:32 ~right_border:32 ~bottom_border:32
      IntRect.({ left = 24 ; top = 24 ; width = 592 ; height = 592 }) yellow_theme_texture
    in
    Texture3x3.add_to_theme texture3x3 theme texture
  in

  let general_theme = Theme.create () in
  let general_theme = setup_label general_theme in
  let general_theme = setup_button general_theme in
  let general_theme = setup_panel general_theme in
  let general_theme = setup_frame general_theme in
  
  let window_frame_theme = Theme.create () in
  let window_frame_theme = setup_window_frame window_frame_theme in
  let themeset = Theme.Set.create () in
  let themeset = Theme.Set.add themeset general_theme_id general_theme in
  let themeset = Theme.Set.add themeset 
    window_frame_theme_id window_frame_theme in
  themeset



let setup_grey_themeset () =
  let setup_label theme =
    let theme = Theme.add_font theme Theme.GlobalAttribute.font font in
    let theme = Theme.add_int theme Theme.GlobalAttribute.character_size 30 in
    let theme = Theme.add_color 
        theme Label.LabelAttribute.text_color (OcsfmlGraphics.Color.black) in
    theme
  in
  let setup_button theme =     
    let normal = Texture3x3.create_from_subrect
      ~left_border:18 ~right_border:18 ~bottom_border:18 ~top_border:18 
      IntRect.({ left = 139 ; top = 40 ; width = 145 ; height = 48}) grey_theme_texture 
    in
    let hovered = Texture3x3.create_from_subrect
      ~left_border:18 ~right_border:18 ~bottom_border:18 ~top_border:18 
      IntRect.({ left = 139 ; top = 104 ; width = 145 ; height = 48}) grey_theme_texture
    in
    let clicked = Texture3x3.create_from_subrect
      ~left_border:18 ~right_border:18 ~bottom_border:18 ~top_border:18 
      IntRect.({ left = 139 ; top = 168 ; width = 145 ; height = 48}) grey_theme_texture
    in
    Button.ButtonAttribute.setup_textured_button theme 
      ~normal ~hovered ~clicked
  in
  let setup_panel theme =
    Theme.add_color theme Panel.PanelAttribute.background_color
      (OcsfmlGraphics.Color.rgb 190 190 210)
  in
  let setup_frame theme =
    let open Frame.FrameAttribute in
    let theme = Theme.add_bool theme use_texture true in
    let texture3x3 = Texture3x3.create_from_subrect 
      ~top_border:4 ~left_border:4 ~right_border:4 ~bottom_border:4
      IntRect.({ left = 24 ; top = 24 ; width = 268 ; height = 268 }) grey_theme_texture
    in
    Texture3x3.add_to_theme texture3x3 theme texture
  in
  let setup_window_frame theme =
    let open Frame.FrameAttribute in
    let theme = Theme.add_bool theme use_texture true in
    let texture3x3 = Texture3x3.create_from_subrect 
      ~top_border:8 ~left_border:8 ~right_border:8 ~bottom_border:8
      IntRect.({ left = 8 ; top = 8 ; width = 298 ; height = 298 }) grey_theme_texture
    in
    Texture3x3.add_to_theme texture3x3 theme texture
  in
  let general_theme = Theme.create () in
  let general_theme = setup_label general_theme in
  let general_theme = setup_button general_theme in
  let general_theme = setup_panel general_theme in
  let general_theme = setup_frame general_theme in
  
  let window_frame_theme = Theme.create () in
  let window_frame_theme = setup_window_frame window_frame_theme in
  let themeset = Theme.Set.create () in
  let themeset = Theme.Set.add themeset general_theme_id general_theme in
  let themeset = Theme.Set.add themeset 
    window_frame_theme_id window_frame_theme in
  themeset






(*let setup_theme () =
  let setup_label theme =
    let theme = Theme.add_font theme Theme.GlobalAttribute.font font in
    let theme = Theme.add_int theme Theme.GlobalAttribute.character_size 12 in
    let theme = Theme.add_color 
        theme Label.LabelAttribute.text_color (OcsfmlGraphics.Color.black) in
    theme
  in
  let setup_button = Button.ButtonAttribute.setup_defaults in
  let setup_frame theme =
    let open Frame.FrameAttribute in
    let theme = Theme.add_bool theme use_texture false in
    let theme = Theme.add_color theme color OcsfmlGraphics.Color.blue in
    theme
  in
  let (theme,id) = Theme.create () in
  let theme = setup_label theme in
  let theme = setup_button theme in
  let theme = setup_frame theme in
  (theme,id)
*)

let _ = 
  let win = new render_window (VideoMode.create ()) "Label" in
  win#set_framerate_limit 60 ;

  let yellow_themeset = setup_yellow_themeset () in
  let grey_themeset = setup_grey_themeset () in
  let current_themeset = ref yellow_themeset in

  let title = new Label.label ~text:"Menu" general_theme_id in
  let title_frame = new Frame.frame ~margins:10. 
    ~child:(title :> Widget.widget) general_theme_id in

  let solo_text = new Label.label ~text:"Solo" general_theme_id in
  let solo_button = new Button.button general_theme_id 
    (solo_text :> Widget.widget) in
  
  let yellow_text = new Label.label ~text:"Yellow theme" general_theme_id in
  let yellow_button = new Button.button general_theme_id 
    ~onClick:(fun _ -> current_themeset := yellow_themeset) 
    (yellow_text :> Widget.widget) in

  let grey_text = new Label.label ~text:"Grey theme" general_theme_id in
  let grey_button = new Button.button general_theme_id 
    ~onClick:(fun _ -> current_themeset := grey_themeset) 
    (grey_text :> Widget.widget) in
  
  
  let close_text = new Label.label ~text:"Quit" general_theme_id in
  let close_button = new Button.button general_theme_id 
   ~onClick:(fun _ -> win#close) (close_text :> Widget.widget) in
  
  
  let main_layout = new Layout.vertical_layout in
  main_layout#add title_frame 2. ;
  main_layout#add Widget.empty_widget 0.1 ;
  main_layout#add solo_button 1. ;
  main_layout#add Widget.empty_widget 0.5 ;
  main_layout#add yellow_button 1. ;
  main_layout#add Widget.empty_widget 0.5 ;
  main_layout#add grey_button 1. ;
  main_layout#add Widget.empty_widget 0.5 ;
  main_layout#add close_button 1. ;
  main_layout#add Widget.empty_widget 0.5 ;


  let frame = new Frame.frame ~margins:34.
    ~child:(main_layout :> Widget.widget) window_frame_theme_id in

  let panel = new Panel.panel ~child:frame general_theme_id in
  panel#resize (400.,600.) ;
  let widget = panel in



  let rec loop () =
    let handle_event (win:#OcsfmlGraphics.render_window) e =
      Event.(match e with
        | Resized { width ; height } ->
            let width,height = (float width, float height) in
            let vrect = FloatRect.({ left = 0.; top = 0.; width; height}) in
            let v = new view (`Rect vrect) in
            win#set_view v ;
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
      widget#draw win !current_themeset ;
      win#display ;
      loop ()
    )
  in loop () ;
  font#destroy ;
  yellow_theme_texture#destroy ;
  grey_theme_texture#destroy
