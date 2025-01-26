open Types

(** [create_quadtree x y width height] crée un quadtree avec les coordonnées (x, y) et les dimensions width et height.
@param x La coordonnée x du coin supérieur gauche du quadtree.
@param y La coordonnée y du coin supérieur gauche du quadtree.
@param width La largeur du quadtree.
@param height La hauteur du quadtree.
@return Un quadtree vide couvrant la zone spécifiée. *)
val create_quadtree : float -> float -> float -> float -> quadtree

(** [retrieve_all quadtree] retourne toutes les briques contenues dans le quadtree.
    @param quadtree Le quadtree à partir duquel récupérer les briques.
    @return Une liste de toutes les briques dans le quadtree. *)
val retrieve_all : quadtree -> brique list

(** [insert brick qt] insère une brique dans le quadtree.
    @param brick La brique à insérer.
    @param qt Le quadtree dans lequel insérer la brique.
    @return Le quadtree mis à jour avec la nouvelle brique. *)
val insert : brique -> quadtree -> quadtree

(** [query qt (Pair (bx, by)) radius] retourne toutes les briques dans le quadtree qui se trouvent dans un rayon donné autour d'un point.
    @param qt Le quadtree à interroger.
    @param Pair (bx, by) Le point central de la zone de recherche.
    @param radius Le rayon de la zone de recherche.
    @return Une liste de briques dans la zone spécifiée. *)
val query : quadtree -> float pair -> float -> brique list

(** [update_quadtree qt updated_briques] met à jour les briques dans le quadtree avec une liste de briques mises à jour.
    @param qt Le quadtree à mettre à jour.
    @param updated_briques La liste des briques mises à jour.
    @return Le quadtree mis à jour avec les nouvelles briques. *)
val update_quadtree : quadtree -> brique list -> quadtree