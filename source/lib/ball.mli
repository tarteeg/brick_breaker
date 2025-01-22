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

module B : Ball