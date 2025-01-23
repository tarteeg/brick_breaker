open Types

  let print_ball balle = match balle with 
    | None -> print_endline "Rien dans la balle"
    | Some (Ball (Pair (x, y), Pair (dx, dy))) ->
      Printf.printf "Ball:\n  Position: (%.2f, %.2f)\n  Velocity: (%.2f, %.2f)\n" x y dx dy

  let print_raquette raquette = match raquette with
    | None -> print_endline "Rien dans la raquette"
    | Some (Raquette (Pair (x, y))) ->
    Printf.printf "Raquette:\n  Position: (%.2f, %.2f)\n" x y

  let print_state state = match state with
    | None -> print_endline "Rien dans l'etat"
    | Some State (Ball (Pair (x, y), Pair (dx, dy)), Raquette raquette) ->
        print_ball (Some (Ball (Pair (x, y), Pair (dx, dy))));
        print_raquette (Some (Raquette raquette));
        print_endline "\n"
