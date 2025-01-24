(* Fichier de débuggage *)

open Types
open List

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
  | Some State (Ball (Pair (x, y), Pair (dx, dy)), Raquette raquette, bricks) ->
      print_ball (Some (Ball (Pair (x, y), Pair (dx, dy))));
      print_raquette (Some (Raquette raquette));
      print_endline "\n"

(* --------------------------------------------------------- *)
(* Vérification des Post-Conditions de "create_briques" (cb) *)
(* --------------------------------------------------------- *)

(* (1) La liste de briques est de taille max_c * max_l *)
let post_cb_size l max_c max_l = (length l) = (max_c * max_l)

(* (2) Il y a max_c briques par ligne *)

(* Fonction auxiliaire qui récupère les positions (x,y) des briques *)
let get_positions l = List.map (fun (Brique (Pair (rx, ry),_, _)) -> (rx, ry)) l

(* Fonction auxiliaire qui récupère la liste des y des briques *)

let post_cb_lines l max_c = 
  let rec aux l = match l with
    | [] -> true
    | h::t -> (length h) = max_c && aux t
  in aux l

(* (3) Il y a max_l briques par colonne *)
