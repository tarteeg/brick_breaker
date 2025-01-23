open Ball

let game_hello () = print_endline "Hello, Newtonoiders!"

let game_launch s = 
  Graphics.open_graph s ;
  Graphics.set_color (Graphics.rgb 168 27 3) ;
  Graphics.fill_rect ((800-100)/2) ((600-20-200)/2) 100 10 ;


  let _ = Graphics.wait_next_event [Key_pressed] in
  Graphics.close_graph ()