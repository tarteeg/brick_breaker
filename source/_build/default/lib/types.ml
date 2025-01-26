type 'a pair = Pair of float * float

type ('pos, 'vel) ball = Ball of 'pos pair * 'vel pair
type raquette = Raquette of float pair

type brique = Brique of float pair * float pair * bool

type quadtree =
  | Node of quadtree * quadtree * quadtree * quadtree * float pair * float pair
  | Leaf of brique list * float pair * float pair

type etat = State : ('pos, 'vel) ball * raquette * quadtree * bool * int -> etat