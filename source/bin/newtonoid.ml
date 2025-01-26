(* Fichier principal *)

open Libnewtonoid (* bibliotheque de modules definis dans lib/ *)
open Iterator
open Game
open Types
(*open Debug*)
open Quadtree

(* Initialisation correcte *)
let () = Graphics.open_graph (Printf.sprintf " %dx%d"
  (int_of_float ((2. *. InitFenetre.marge) +. InitFenetre.supx -. InitFenetre.infx))
  (int_of_float ((2. *. InitFenetre.marge) +. InitFenetre.supy -. InitFenetre.infy)))


(* extrait le score courant d'un etat : 
Score = nombre de briques cassées *)
let score etat = match etat with 
    |Some (State (_, _, quadtree, _, _)) -> 
    let rec aux l = match l with 
      |[] -> 0
      |(Brique (_,_,est_cassee))::q -> 
        if est_cassee then 1 + aux q 
        else aux q 
      in aux (retrieve_all quadtree)
    |None -> failwith "Erreur : etat inconnu"

module Drawing (F : Frame) = 
struct
  (*
  let graphic_format =
    Format.sprintf
      " %dx%d+50+50"
      (int_of_float ((2. *. InitFenetre.marge) +. InitFenetre.supx -. InitFenetre.infx))
      (int_of_float ((2. *. InitFenetre.marge) +. InitFenetre.supy -. InitFenetre.infy))
  
  let draw_fin_de_partie () =
    Graphics.moveto (int_of_float (InitFenetre.infx +. 200.)) (int_of_float (InitFenetre.supy -. InitFenetre.marge -. 20.));
    Graphics.draw_string "Partie Terminee"
  *)

  let draw_nb_balles_restantes nb = 
    Graphics.moveto (int_of_float (InitFenetre.infx +. 100.)) (int_of_float (InitFenetre.supy -. InitFenetre.marge));
    Graphics.draw_string (Printf.sprintf "Balles : %d" nb)

  let draw_score s = 
    Graphics.moveto (int_of_float (InitFenetre.infx)) (int_of_float (InitFenetre.supy -. InitFenetre.marge));
    Graphics.draw_string (Printf.sprintf "Score : %d" s)

  let draw_state etat =
    (* Affichage du score *)
    let score_actuel = score etat in 
    draw_score score_actuel ; (
    match etat with 
      | None -> failwith "Erreur"
      | Some (State (Ball (Pair (x, y), Pair (_,_)), Raquette (Pair (rx,ry)), quadtree, _,nb_balles)) -> 
        begin
          (* Affichage du nombre de balles *)
          draw_nb_balles_restantes nb_balles;

          (* Placement de la balle *)
          Graphics.draw_circle (int_of_float x) (int_of_float y) 5; 

          (* Placement de la raquette *)
          Graphics.fill_rect (int_of_float rx) (int_of_float ry) 100 10 ;

          (* Fonction pour dessiner une brique *)
          let draw_brick brick =
            let Brique (Pair (x, y), Pair (width, height), is_broken) = brick in
            if not is_broken then
              Graphics.fill_rect (int_of_float x) (int_of_float y) (int_of_float width) (int_of_float height)
          in

          (* Récupération de toutes les briques depuis le quadtree *)
          let bricks = retrieve_all quadtree in

          (* Dessin des briques *)
          List.iter draw_brick bricks;
        end
    )

  let draw flux_etat =
    let rec loop flux_etat last_score =
      match Flux.(uncons flux_etat) with
      | None -> last_score
      | Some (etat, flux_etat') ->
        (*Debug.print_state etat;*)

        Graphics.clear_graph ();
        (* DESSIN ETAT *)
        draw_state etat;
        (* FIN DESSIN ETAT *)
        Graphics.synchronize ();
        Unix.sleepf InitFenetre.dt;

        (* Maj du flux *)
        loop (flux_etat') (last_score + score etat);
    in
    (*Graphics.open_graph graphic_format;*)
    Graphics.auto_synchronize false;
    let score = loop flux_etat 0 in
    Format.printf "Score final : %d@\n" score;
    Graphics.close_graph () 
end

(* Initialisation de la fenêtre graphique *)

module G = Game(InitFenetre)
module D = Drawing(InitFenetre)

let _ = 
  let flux_etat =
    Flux.unfold
      (fun state -> 
        let mouse_x = fst (Graphics.mouse_pos ()) |> float_of_int in
        let mouse_pressed = Graphics.button_down () in
        Some (state, G.update_state (mouse_x, mouse_pressed) state))
      InitGame.etat_init in 
  D.draw (flux_etat)