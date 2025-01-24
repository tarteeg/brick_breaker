open Iterator
open Input
open Types

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
  let dt = 1. /. 60. (* 60 Hz *)
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end


module Game (F : Frame) = 
struct

  (*update ball*)
  let update_ball ball = match ball with 
  |Ball (Pair (bx,by), Pair (vx,vy)) -> 

    Ball (Pair (bx+.vx*.F.dt, by+.vy*.F.dt), Pair (vx,vy))
  |_ -> failwith "Erreur : ball inconnue"

  (*update raquette*)
  let update_raquette mouse_x raquette = match raquette with 
  |Raquette (Pair (rx,ry)) -> 
    let new_rx = max 10. (min (790. -. 100.) (mouse_x -. 50.)) in
    Raquette (Pair (new_rx, ry))
  |_ -> failwith "Erreur : raquette inconnue"


  let collisions_murs (Ball (Pair (bx,by), Pair (dx,dy))) = 
    let new_dx = 
      if bx < Init.infx || bx > Init.supx then -.dx else dx in
    let new_dy = 
      if by < Init.infy || by > Init.supy then -.dy else dy in
    Ball (Pair (bx,by), Pair (new_dx, new_dy))
  
  let collisions_raquette (Ball (Pair (bx,by), Pair (vx,vy))) (Raquette (Pair (rx,ry))) = 
    if by >= ry && by <= ry +. 10. && bx >= rx && bx <= rx +. 100. then
      let relative_x = (bx -. rx) /. 100. in  (* Position relative sur la raquette *)
      let angle = (relative_x -. 0.5) *. 1.0 in  (* Angle de rebond basé sur la position *)
      let speed = sqrt (vx *. vx +. vy *. vy) in
      let new_vx = speed *. sin angle in
      let new_vy = abs_float (speed *. cos angle) in  (* On s'assure que la balle part vers le haut *)
      Ball (Pair (bx,by), Pair (new_vx, new_vy))
    else Ball (Pair (bx,by), Pair (vx, vy))

  let update_state (mouse_x, _) state = match state with 
    | Some (State ((Ball ((Pair (bx,by)), (Pair (dx,dy)))), Raquette (Pair (rx,ry)))) ->

      (*Mise à jour de la raquette*)
      let new_raquette = update_raquette mouse_x (Raquette (Pair (rx,ry))) in

      (*Mise à jour de la balle*)
      let new_ball = update_ball (Ball (Pair (bx,by), Pair (dx,dy))) in

      (*Mise à jour des collisions avec les murs*)
      let ball_after_walls = collisions_murs new_ball in

      (*Mise à jour des collisions avec la raquette*)
      let ball_after_raquette = collisions_raquette ball_after_walls new_raquette in


      Some (State (ball_after_raquette, new_raquette))
    | None -> failwith "Erreur : etat inconnu"
    | _ -> failwith "Erreur : etat inconnu"
  
end