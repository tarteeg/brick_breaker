open Iterator

(* Module abstrayant une balle *)
module type Ball = 
sig
    type p

    (* Type représentant l'état de la balle *)
    type ball_state

    val radius : int

    val p_initiale : p
    val s_initiale : p
    val a_initiale : p
    (* val init_ball : unit -> unit *)

    val position : p flux
    val speed : p flux
    val acceleration : p flux

    (* val print_ball : (float * float) flux -> unit *)
end

module B = struct

  type pair = (float * float)

  (* Type représentant l'état de la balle
  i.e. la paire ((x,y),(dx,dy))  
  où (x,y) est la position
  et (dx,dy) est la vitesse *)
  type ball_state =  pair * pair 

  let radius = 4
  let p_initiale = (400.,300.)
  let s_initiale = (0.,0.)
  let a_initiale = (0.,0.)
  (* 
  let init_ball = 
    pos 
  *) 
  let position = Flux.(cons p_initiale vide)
  let speed = Flux.(cons s_initiale vide)
  let acceleration = Flux.(cons a_initiale vide)

end