(* Fichier de débuggage *)
open Types

(* Affichage de l'état de la balle *)
let print_ball balle = match balle with 
  | None -> print_endline "Rien dans la balle"
  | Some (Ball (Pair (x, y), Pair (dx, dy))) ->
    Printf.printf "Ball:\n  Position: (%.2f, %.2f)\n  Velocity: (%.2f, %.2f)\n" x y dx dy

(* Affichage de l'état de la raquette *)
let print_raquette raquette = match raquette with
  | None -> print_endline "Rien dans la raquette"
  | Some (Raquette (Pair (x, y))) ->
  Printf.printf "Raquette:\n  Position: (%.2f, %.2f)\n" x y

(* Affichage de l'état de la balle et de la raquette *)
let print_state state = match state with
  | None -> print_endline "Rien dans l'etat"
  | Some State (Ball (Pair (x, y), Pair (dx, dy)), Raquette raquette, bricks, partie_en_cours, nb_balles) ->
      print_ball (Some (Ball (Pair (x, y), Pair (dx, dy))));
      print_raquette (Some (Raquette raquette));
      print_endline "\n"

