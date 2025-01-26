type 'a pair = Pair of float * float

type ('pos, 'vel) ball = Ball of 'pos pair * 'vel pair
type raquette = Raquette of float pair

type brique = Brique of float pair * float pair * bool

type etat = State : ('pos, 'vel) ball * raquette * brique list * bool * int -> etat

