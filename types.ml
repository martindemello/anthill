type group = Group.CharSet.t;;

type bag = int Bag.MultiSet.t

type node = Letter of char | Group of group | Dot | Star


