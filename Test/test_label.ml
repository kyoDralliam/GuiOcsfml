open OcsfmlWindow
open OcsfmlGraphics


let _ = 
  let win = new render_window (VideoMode.create ()) "Label" in

  let button = Button.create ~onClick:(fun () -> win#close) () in
  let label = Label.create ~text:"Hello Gui Ocsfml" () in
  let widget = Widget.Button (button, Widget.Label label) in



  let rec loop () =
    let handle_event e =
      Event.(match e with
        | Resized { width ; height } ->
            Widget.resize widget (float width, float height)
        | KeyPressed { code = KeyCode.Escape ; _ } -> win#close
        | _ -> ignore (Widget.handle_event widget e)
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
      Widget.draw widget (win :> OcsfmlGraphics.render_target) ;
      win#display ;
      loop ()
    )
  in loop () 
