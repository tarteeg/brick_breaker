open Iterator
open Input
open Types

module type Frame =
  sig
    (* Fenêtre principale *)
    val dt : float
    val marge : float 
    val infx : float
    val infy : float
    val supx : float
    val supy : float
  end

(* Parametres globaux d'initialisation du jeu *)
module InitFenetre : Frame = struct
  let dt = 1. /. 60. (* 60 Hz *)
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

(* Module définissant un couple de flottants (pour les objets balle et raquette) *)
module type InitPair =
sig 
  val x : float
  val y : float 
end 

(* Position initiale de la balle *)
module InitPosBalle : InitPair = struct
  let x = 400.
  let y = 300.
end

(* Vitesse initiale de la balle *)
module InitVelocityBalle : InitPair = struct
  let x = 300.
  let y = -300.
end

(* Position initiale de la raquette *)
module InitPosRaquette : InitPair = struct
  let x = 400.
  let y = 50.
end

(* Taile initiale de la raquette *)
module InitTailleRaquette : InitPair = struct
  let x = 100. (* Largeur *)
  let y = 10. (* Hauteur *)
end

(* Initialisation de la partie *)
module InitGame = struct

  (* Initialisation de la balle *)
  let pos_balle = Pair (InitPosBalle.x, InitPosBalle.y)
  let vel_balle = Pair (InitVelocityBalle.x, InitVelocityBalle.y)
  let balle = Ball (pos_balle, vel_balle)
  
  (* Initialisation de la raquette *)
  let pos_raquette = Pair (InitPosRaquette.x, InitPosRaquette.y)
  let raquette = Raquette pos_raquette

  (* Initialisation des briques *)
  let briques = [
    create_brick 100. 500. 70. 20.;
    create_brick 200. 500. 70. 20.;
    create_brick 300. 500. 70. 20.;
    create_brick 400. 500. 70. 20.;
  ]

  let etat_init = Some (State (balle, raquette, briques))
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
      if bx < InitFenetre.infx || bx > InitFenetre.supx then -.dx else dx in
    let new_dy = 
      if by < InitFenetre.infy || by > InitFenetre.supy then -.dy else dy in
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

  (* Gestion des collisions avec les briques *)
  let handle_collision ball brick =
    let Ball (Pair (bx, by), Pair (vx, vy)) = ball in
    let Pair (rx, ry) = brick.position in
    let brick_width = brick.width in
    let brick_height = brick.height in
  
    if is_colliding ball brick then
      (* Déterminer si la collision est sur un bord horizontal ou vertical *)
      let collided_from_top_or_bottom =
        bx >= rx && bx <= rx +. brick_width &&
        (abs_float (by -. ry) <= 5.0 || abs_float (by -. (ry +. brick_height)) <= 5.0)
      in
  
      let collided_from_left_or_right =
        by >= ry && by <= ry +. brick_height &&
        (abs_float (bx -. rx) <= 5.0 || abs_float (bx -. (rx +. brick_width)) <= 5.0)
      in
  
      (* Ajuster les directions *)
      let new_vx = if collided_from_left_or_right then -.vx else vx in
      let new_vy = if collided_from_top_or_bottom then -.vy else vy in
  
      let new_ball = Ball (Pair (bx, by), Pair (new_vx, new_vy)) in
      (new_ball, break_brick brick)  (* Marquer la brique comme cassée *)
    else
      (ball, brick)
  
  let collisions_briques ball bricks =
    let rec process_bricks ball bricks updated_bricks =
      match bricks with
      | [] -> (ball, List.rev updated_bricks)
      | brick :: rest ->
        let ball_after_collision, updated_brick = handle_collision ball brick in
        process_bricks ball_after_collision rest (updated_brick :: updated_bricks)
    in
    process_bricks ball bricks []

  let update_state (mouse_x, _) state = match state with 
    | Some (State ((Ball ((Pair (bx,by)), (Pair (dx,dy)))), Raquette (Pair (rx,ry)), bricks)) ->

      (*Mise à jour de la raquette*)
      let new_raquette = update_raquette mouse_x (Raquette (Pair (rx,ry))) in

      (*Mise à jour de la balle*)
      let new_ball = update_ball (Ball (Pair (bx,by), Pair (dx,dy))) in

      (*Mise à jour des collisions avec les murs*)
      let ball_after_walls = collisions_murs new_ball in

      (*Mise à jour des collisions avec la raquette*)
      let ball_after_raquette = collisions_raquette ball_after_walls new_raquette in

      let ball_after_briques, new_bricks = collisions_briques ball_after_raquette bricks in

      Some (State (ball_after_briques, new_raquette, new_bricks))
    | None -> failwith "Erreur : etat inconnu"
    | _ -> failwith "Erreur : etat inconnu"
end