open Types

val create_quadtree : float -> float -> float -> float -> quadtree
val retrieve_all : quadtree -> brique list
val insert : brique -> quadtree -> quadtree
val query : quadtree -> float pair -> float -> brique list
val update_quadtree : quadtree -> brique list -> quadtree