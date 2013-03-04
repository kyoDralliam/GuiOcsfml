open OcsfmlWindow
open OcsfmlGraphics


let _ = 
  let win = new render_window (VideoMode.create ()) "Label" in

  let label1 = new Label.label ~text:"Hello Gui Ocsfml" () in
  let button1 = new Button.button 
    ~onClick:(fun () -> print_string "Hello\n" ; flush stdout)
    (label1 :> Widget.widget) 
  in

  let label2 = new Label.label ~text:"Goodbye Gui Ocsfml" () in
  let button2 = new Button.button ~onClick:(fun () -> win#close) (label2 :> Widget.widget) in


  let label3 = new Label.label ~text:"Do nothing" () in
  let button3 = new Button.button (label3 :> Widget.widget) in


  let layout = new Layout.vertical_layout in
  layout#add button2 15. ;
  layout#add button1 5. ;
  layout#add button3 15. ;


  let widget = layout in



  let rec loop () =
    let handle_event e =
      Event.(match e with
        | Resized { width ; height } ->
            widget#resize (float width, float height)
        | KeyPressed { code = KeyCode.Escape ; _ } -> win#close
        | _ -> ignore (widget#onEvent e)
      )
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
      widget#draw win ;
      win#display ;
      loop ()
    )
  in loop () 
