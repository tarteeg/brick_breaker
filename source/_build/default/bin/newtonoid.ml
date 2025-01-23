(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator

(* exemple d'ouvertue d'un tel module de la bibliotheque : *)
open Game
open Types
open Debug

module type Frame =
  sig
    val dt : float
    val marge : float 
    val infx : float
    val infy : float
    val supx : float
    val supy : float
  end

(* Parametres globaux d'initialisation du jeu *)
(* dt : pas de temps                          *)
(* marge : paire d'abscisses (xmin, xmax)     *)
(* infx : paire d'abscisses (xmin, xmax)      *)
(* infy : paire d'abscisses (xmin, xmax)      *)
(* supx : paire d'abscisses (xmin, xmax)      *)
(* supy : paire d'abscisses (xmin, xmax)      *)
module Init : Frame = struct
  let dt = 0.01 (* 60 Hz *)
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

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

let g = 9.81
module Game (F : Frame) = 
struct
  let update_ball ball = match ball with 
  |Ball (Pair (bx,by), Pair (vx,vy)) -> 

    let new_vx = 
      if bx < Init.infx || bx > Init.supx then -.vx else vx in
    let new_vy = 
      if by > Init.supy then -.vy else vy in
    
    Ball (Pair (bx+.new_vx,by+.new_vy), Pair (new_vx,new_vy))
  |_ -> failwith "Erreur : ball inconnue"

  let update_raquette raquette = match raquette with 
  |Raquette (Pair (rx,ry)) -> Raquette (Pair (rx,ry))
  |_ -> failwith "Erreur : raquette inconnue"

  let update_state state = match state with 
    | Some (State ((Ball ((Pair (bx,by)), (Pair (dx,dy)))), Raquette (Pair (rx,ry)))) ->
      Some (State (update_ball (Ball (Pair (bx,by), Pair (dx,dy))), update_raquette (Raquette (Pair (rx,ry)))))
    | None -> failwith "Erreur : etat inconnu"
    | _ -> failwith "Erreur : etat inconnu"

  let ( |+| ) (Pair (x1,y1)) (Pair (x2,y2)) = Pair (x1 +. x2, y1 +. y2)
  let run : etat -> etat option Flux.t = 
    fun (State (Ball (pos0,vit0), raquette)) -> 
      let acceleration = Flux.constant (Pair (0.,-.g)) in
      let vitesse = Flux.(map (( |+| ) vit0) (integre F.dt acceleration)) in
      let position = Flux.(map (( |+| ) pos0) (integre F.dt vitesse)) in
      Flux.map2 (fun p v -> Some (State (Ball (p,v), raquette))) position vitesse
end

module Drawing (F : Frame) = 
struct
  let graphic_format =
    Format.sprintf
      " %dx%d+50+50"
      (int_of_float ((2. *. Init.marge) +. Init.supx -. Init.infx))
      (int_of_float ((2. *. Init.marge) +. Init.supy -. Init.infy))

  (* extrait le score courant d'un etat : *)
  let score etat : int = 0 
  
  let draw_state etat =
    match etat with 
      | None -> failwith "Erreur"
      | Some (State (Ball (Pair (x, y), Pair (_,_)), Raquette (Pair (rx,ry)))) -> 
        begin
          (* Placement de la balle *)
          Graphics.draw_circle (int_of_float x) (int_of_float y) 5; 

          (* Placement de la raquette *)
          Graphics.fill_rect (int_of_float rx) (int_of_float ry) 100 10 ;
        end
    (* failwith "A DEFINIR" *)

  let draw flux_etat =
    let rec loop flux_etat last_score =
      match Flux.(uncons flux_etat) with
      | None -> last_score
      | Some (etat, flux_etat') ->
        Debug.print_state etat;

        Graphics.clear_graph ();
        (* DESSIN ETAT *)
        draw_state etat;
        (* FIN DESSIN ETAT *)
        Graphics.synchronize ();
        Unix.sleepf Init.dt;
        
        (* Maj du flux *)
        loop (flux_etat') (last_score + score etat);
      | _ -> assert false
    in
    Graphics.open_graph graphic_format;
    Graphics.auto_synchronize false;
    let score = loop flux_etat 0 in
    (* Format.printf "Score final : %d@\n" score; *)
    Graphics.close_graph () 
end

(* Initialisation *)
let pos_balle = Pair (400., 300.)
let vel_balle = Pair (5., -3.)
let balle = Ball (pos_balle, vel_balle)

let pos_raquette = Pair (400., 50.)
let raquette = Raquette pos_raquette

(*
let briques = Brick [
  Pair (100., 500.); 
  Pair (200., 500.); 
  Pair (300., 500.);
  Pair (400., 500.)
]
*)
let etat0 = Some (State (balle, raquette))

module G = Game(Init)
module D = Drawing(Init)

let _ = 
  let flux_etat = Flux.unfold (fun state -> Some (state, G.update_state state)) etat0 in 
  D.draw (flux_etat)

(*
  D.draw(G.run(etat0))
*)

(* let _ = (game_launch graphic_format) *)