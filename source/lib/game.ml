open Iterator
open Input
open Types
open Quadtree

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
  let supx = 990.
  let supy = 590.
end

(* Module définissant un couple de flottants (pour les objets balle,raquette et brique) *)
module type InitPair =
sig 
  val x : float
  val y : float 
end 

(* Position initiale de la balle *)
module InitPosBalle : InitPair = struct
  let x = (InitFenetre.infx +. InitFenetre.supx) /. 2.
  let y = (InitFenetre.infy +. InitFenetre.supy) /. 2.
end

(* Vitesse initiale de la balle *)
module InitVelocityBalle : InitPair = struct
  let x = 0.
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

(* Taille standard d'une brique *)
(* On fixe la taille d'une brique puis on ajuste le nombre de briques par lignes (nous aurions pu faire l'inverse) *)
module InitTailleBriques : InitPair = struct
  let x = 70. (* Largeur *)
  let y = 20. (* Hauteur *)
end

(* Constantes sur les briques *)
module ConstantesBriques = struct 
  let espace_briques = 2. 
  let max_c = int_of_float ((InitFenetre.supx -. InitFenetre.infx -. InitFenetre.marge) /. (InitTailleBriques.x +. espace_briques))
  let max_l = int_of_float ((InitFenetre.supy /. 2. -. InitFenetre.infy -. InitFenetre.marge) /. (InitTailleBriques.y +. espace_briques))
  let nb_briques = max_l * max_c
end 

(* Initialisation de la partie *)
module InitGame = struct
  (* Nombre de balles*)
  let nb_balles = 3

  (* Initialisation de la balle *)
  let pos_balle = Pair (InitPosBalle.x, InitPosBalle.y)
  let vel_balle = Pair (InitVelocityBalle.x, InitVelocityBalle.y)
  let balle = Ball (pos_balle, vel_balle)
  
  (* Initialisation de la raquette *)
  let pos_raquette = Pair (InitPosRaquette.x, InitPosRaquette.y)
  let raquette = Raquette pos_raquette


  (* CONTRAT
  Fonction qui crée les briques selon un pattern "rectangulaire" sur la fenêtre de jeu
  Argument max_c : int : nombre de colonnes  
  Argument max_l : int : nombre de lignes 
  Préconditions : max_c >= 0 ET max_l >= 0
  Postconditions : 
      (1) La liste de briques est de taille max_c * max_l
      (2) Il y a max_c briques par ligne
      (3) Il y a max_l briques par colonne
      (4) Aucune briques ne se chevauchent
  *)
  let rec create_briques =
    let create_brick x y width height = Brique (Pair (x, y), Pair (width, height), false) in
    let rec aux col_actuelle ligne_actuelle l =
      if ligne_actuelle = ConstantesBriques.max_l
 then l
      else if col_actuelle = ConstantesBriques.max_c then aux 0 (ligne_actuelle + 1) l
      else
        let total_width = (float_of_int ConstantesBriques.max_c) *. (InitTailleBriques.x +. ConstantesBriques.espace_briques) -. ConstantesBriques.espace_briques in
        let x = (InitFenetre.supx -. InitFenetre.infx -. total_width) /. 2. +. InitFenetre.infx +. (float_of_int col_actuelle) *. (InitTailleBriques.x +. ConstantesBriques.espace_briques) in
        let y = InitFenetre.supy /. 2. +. InitFenetre.marge +. (float_of_int ligne_actuelle) *. (InitTailleBriques.y +. ConstantesBriques.espace_briques) in
        aux (col_actuelle + 1) ligne_actuelle (create_brick x y InitTailleBriques.x InitTailleBriques.y :: l)
    in
    aux 0 0 []

  let briques = create_briques

  let quadtree = List.fold_left (fun acc brique -> insert brique acc) (create_quadtree InitFenetre.infx InitFenetre.infy (InitFenetre.supx -. InitFenetre.infx) (InitFenetre.supy -. InitFenetre.infy)) briques

  let etat_init = Some (State (balle, raquette, quadtree, false, nb_balles))
end 

(* --------------------------------------------------------- *)
(* Vérification des Post-Conditions de "create_briques" (cb) *)
(* --------------------------------------------------------- *)
module PostCondCB = struct
  open List

  (* (1) La liste de briques est de taille max_columns * max_lines *)
  let post_cb_size l = (length l) = ConstantesBriques.max_c * ConstantesBriques.max_l

  (* (2) Il y a max_c briques par ligne *)

  (* Fonction auxiliaire qui récupère les positions (x,y) des briques *)
  let get_positions l = List.map (fun (Brique (Pair (rx, ry),_, _)) -> (rx, ry)) l

  (* Fonctions auxiliaires qui récupèrent la liste des x et des y des briques *)
  let get_x l = List.map (fun (x, _) -> x) l
  let get_y l = List.map (fun (_, y) -> y) l

  (* Fonction qui vérifie si les briques sont alignées sur une ligne *)

  let post_cb_lines l = 
    let rec aux l = match l with
      | [] -> true
      | h::t -> (length h) = ConstantesBriques.max_c
 && aux t
    in aux l

  (* (3) Il y a max_l briques par colonne *)

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
  | Raquette (Pair (rx, ry)) -> 
    let new_rx = max F.infx (min (F.supx -. InitTailleRaquette.x) (mouse_x -. InitTailleRaquette.x /. 2.)) in
    Raquette (Pair (new_rx, ry))
  | _ -> failwith "Erreur : raquette inconnue"

  let collisions_murs (Ball (Pair (bx,by), Pair (dx,dy))) = 
    if by < F.infy then
      Ball (Pair (InitPosBalle.x, InitPosBalle.y), Pair (InitVelocityBalle.x, InitVelocityBalle.y))
    else
      let new_dx = 
        if bx < F.infx || bx > F.supx then -.dx else dx in
      let new_dy = 
        if by > F.supy then -.dy else dy in
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

  (* Vérifie si une balle entre en collision avec une brique *)
  let is_colliding (Ball (Pair (bx, by), _)) brick = match brick with
    | Brique (Pair (x, y), Pair (w, h), is_broken) ->
      let ball_radius = 5.0 in (* Rayon de la balle *)

      (* Trouver le point le plus proche de la balle sur la brique *)
      let closest_x = max x (min bx (x +. w)) in
      let closest_y = max y (min by (y +. h)) in

      (* Calculer la distance entre le centre de la balle et ce point *)
      let distance_x = bx -. closest_x in
      let distance_y = by -. closest_y in

      (* Collision si la distance est inférieure ou égale au rayon *)
      (distance_x ** 2. +. distance_y ** 2.) <= ball_radius ** 2. && not is_broken

  let break_brick brique = match brique with 
    | Brique (Pair (x, y), Pair (w, h), _) -> Brique (Pair (x, y), Pair (w, h), true)
    | _ -> failwith "Erreur : brique inconnue"
    
  (* Gestion des collisions avec les briques *)
  let handle_collision ball brick =
    let Ball (Pair (bx, by), Pair (vx, vy)) = ball in
    let Brique (Pair (rx, ry), Pair (brick_width, brick_height), _) = brick in

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
  
  let lose ball = match ball with
    | Ball (Pair (_, by), _) -> by <= F.infy

  let rec update_state (mouse_x, mouse_pressed) state = match state with 
    | Some (State ((Ball ((Pair (bx,by)), (Pair (dx,dy)))), Raquette (Pair (rx,ry)), quadtree, partie_en_cours, nb_balles)) ->
      if not partie_en_cours then
        let new_raquette = update_raquette mouse_x (Raquette (Pair (rx,ry))) in
        if mouse_pressed then 
          (Some (State (InitGame.balle, new_raquette, quadtree, true, nb_balles)))
        else 
          Some (State ((Ball ((Pair (bx,by)), (Pair (dx,dy)))), new_raquette, quadtree, false, nb_balles))
      else
        (*Mise à jour de la balle*)
        let new_ball = update_ball (Ball (Pair (bx,by), Pair (dx,dy))) in

        (* Vérification de perte de balle *)
        let defaite = lose new_ball in
        if defaite then 
          let new_raquette = update_raquette mouse_x (Raquette (Pair (rx,ry))) in

          (* Vérification de fin de partie (plus de balles) *)
          if nb_balles = 1 then 
            Some (State (InitGame.balle, InitGame.raquette, InitGame.quadtree, false, InitGame.nb_balles))      
          else  
            Some (State (InitGame.balle, new_raquette, quadtree, false, nb_balles - 1))
        else 
          (*Mise à jour de la raquette*)
          let new_raquette = update_raquette mouse_x (Raquette (Pair (rx,ry))) in

          (*Mise à jour des collisions avec les murs*)
          let ball_after_walls = collisions_murs new_ball in

          (*Mise à jour des collisions avec la raquette*)
          let ball_after_raquette = collisions_raquette ball_after_walls new_raquette in

          (* Extraire la position de la balle (bx, by) *)
          let Ball (Pair (bx, by), _) = ball_after_raquette in

          (* Query pour récupérer les briques proches de la balle *)
          let radius = 50.0 in  (* rayon de recherche autour de la balle *)
          let briques_proches = query quadtree (Pair (bx, by)) radius in


          (* On met à jour les briques proches de la balle et on teste les collisions *)
          let rec update_briques_and_ball ball bricks updated_briques =
            match bricks with
            | [] -> ball, updated_briques
            | b :: bs ->
              let ball, updated_brique = handle_collision ball b in
              update_briques_and_ball ball bs (updated_brique :: updated_briques)
          in

          let ball_after_briques, updated_briques = update_briques_and_ball ball_after_raquette briques_proches [] in

          (* Mise à jour du quadtree avec les briques mises à jour *)
          let updated_quadtree = update_quadtree quadtree updated_briques in

          Some (State (ball_after_briques, new_raquette, updated_quadtree, true, nb_balles))
    | None -> failwith "Erreur : etat inconnu"
    | _ -> failwith "Erreur : etat inconnu"
end



