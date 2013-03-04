open OcsfmlWindow
open OcsfmlGraphics


class empty_widget =
object
  inherit Widget.widget (Geometry.create (0.,0.) (0.,0.))

  method onEvent _ = false
  method draw _ = ()
end

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
  layout#add (new empty_widget) 0.25 ;
  layout#add button2 15. ;
  layout#add (new empty_widget) 0.25 ;
  layout#add button1 5. ;
  layout#add (new empty_widget) 0.25 ;
  layout#add button3 15. ;
  layout#add (new empty_widget) 0.25 ;


  let widget = layout in



  let rec loop () =
    let handle_event e =
      Event.(match e with
        | Resized { width ; height } ->
            let width,height = (float width, float height) in
            let vrect = OcsfmlGraphics.({ left = 0.; top = 0.; width; height}) in
            let v = new OcsfmlGraphics.view (`Rect vrect) in
            win#set_view v ;
            widget#resize (width, height)
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
