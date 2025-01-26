(* Fichier principal *)

open Libnewtonoid (* bibliotheque de modules definis dans lib/ *)
open Iterator
open Game
open Types
open Debug

(* Initialisation correcte *)
let () = Graphics.open_graph (Printf.sprintf " %dx%d"
  (int_of_float ((2. *. InitFenetre.marge) +. InitFenetre.supx -. InitFenetre.infx))
  (int_of_float ((2. *. InitFenetre.marge) +. InitFenetre.supy -. InitFenetre.infy)))

(* Fonction qui intègre/somme les valeurs successives du flux *)
(* avec un pas de temps dt et une valeur initiale nulle, i.e. *)
(* acc_0 = 0; acc_{i+1} = acc_{i} + dt * flux_{i}             *)
(* paramètres:                                                *)
(* dt : float                                                 *)
(* flux : (float * float) Flux.t                              *)
let integre dt flux =
  (* valeur initiale de l'intégrateur                         *)
  let init = Pair ( 0., 0.) in
  (* fonction auxiliaire de calcul de acc_{i} + dt * flux_{i}
  
  *)
  let iter (Pair (acc1, acc2)) (Pair (flux1, flux2)) =
    Pair (acc1 +. dt *. flux1, acc2 +. dt *. flux2) in
  (* définition récursive du flux acc                         *)
  let rec acc =
    Tick (lazy (Some (init, Flux.map2 iter acc flux)))
  in acc;;

(* extrait le score courant d'un etat : 
Score = nombre de briques cassées *)
let score etat = match etat with 
    |Some (State (_, _, bricks, _, _)) -> 
    let rec aux l = match l with 
      |[] -> 0
      |(Brique (_,_,est_cassee))::q -> 
        if est_cassee then 1 + aux q 
        else aux q 
      in aux bricks
    |None -> failwith "Erreur : etat inconnu"

module Drawing (F : Frame) = 
struct
  let graphic_format =
    Format.sprintf
      " %dx%d+50+50"
      (int_of_float ((2. *. InitFenetre.marge) +. InitFenetre.supx -. InitFenetre.infx))
      (int_of_float ((2. *. InitFenetre.marge) +. InitFenetre.supy -. InitFenetre.infy))

  let draw_fin_de_partie () =
    Graphics.moveto (int_of_float (InitFenetre.infx +. 200.)) (int_of_float (InitFenetre.supy -. InitFenetre.marge -. 20.));
    Graphics.draw_string "Partie Terminee"

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
      | Some (State (Ball (Pair (x, y), Pair (_,_)), Raquette (Pair (rx,ry)), bricks, partie_en_cours,nb_balles)) -> 
        begin
          (* Affichage du nombre de balles *)
          draw_nb_balles_restantes nb_balles;

          (* Placement de la balle *)
          Graphics.draw_circle (int_of_float x) (int_of_float y) 5; 

          (* Placement de la raquette *)
          Graphics.fill_rect (int_of_float rx) (int_of_float ry) 100 10 ;

          let draw_brick brick =
            let Brique (Pair (x, y), Pair (width, height), is_broken) = brick in
            if not is_broken then
              Graphics.fill_rect (int_of_float x) (int_of_float y) (int_of_float width) (int_of_float height)
          in
          let draw_bricks bricks =
            List.iter draw_brick bricks
          in
          draw_bricks bricks;
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
      | _ -> assert false
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