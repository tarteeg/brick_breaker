type 'a pair = Pair of float * float

type ('pos, 'vel) ball = Ball of 'pos pair * 'vel pair
type raquette = Raquette of float pair
type briques = Brick of float pair list

type etat = 
  | State : ('pos, 'vel) ball * raquette -> etat