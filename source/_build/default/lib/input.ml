open Iterator

(* flux de paires (abscisse souris, booléen vrai si bouton appuyé) *)
let mouse =
  Flux.unfold
    (fun () ->
      let x, _ = Graphics.mouse_pos () in
      Some ((float_of_int x, Graphics.button_down ()), ()))
    ()

(* Simule un mouvement de souris vers la droite *)
let simulated_mouse =
  Flux.unfold
    (fun x ->
       if x < 800. then Some ((x, false), x +. 5.)
       else None)
    0.