type 'a pair = Pair of float * float

type ('pos, 'vel) ball = Ball of 'pos pair * 'vel pair
type raquette = Raquette of float pair


type 'pos brick = {
  position : 'pos pair;
  width : float;
  height : float;
  is_broken : bool;
}

let create_brick x y width height = {
  position = Pair (x, y);
  width;
  height;
  is_broken = false;
}

let break_brick brick = { brick with is_broken = true }

(* Vérifie si une balle (cercle) entre en collision avec une brique (rectangle) *)
let is_colliding (Ball (Pair (bx, by), _)) brick =
  let Pair (rx, ry) = brick.position in
  let brick_width = brick.width in
  let brick_height = brick.height in
  let ball_radius = 5.0 in (* Rayon de la balle *)

  (* Trouver le point le plus proche de la balle sur la brique *)
  let closest_x = max rx (min bx (rx +. brick_width)) in
  let closest_y = max ry (min by (ry +. brick_height)) in

  (* Calculer la distance entre le centre de la balle et ce point *)
  let distance_x = bx -. closest_x in
  let distance_y = by -. closest_y in

  (* Collision si la distance est inférieure ou égale au rayon *)
  (distance_x ** 2. +. distance_y ** 2.) <= ball_radius ** 2. && not brick.is_broken


type etat = 
  | State : ('pos, 'vel) ball * raquette * 'pos brick list -> etat