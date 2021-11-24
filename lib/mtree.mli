type h 
val ( ^ ) : h -> h -> h
type 'a mt = Leaf of 'a |  Node of h * 'a mt * 'a mt
val print_hash : h -> string
val hash : 'b -> h
val dhash_tree : 'a mt -> 'a mt -> h
val get_hash : 'a mt -> h
val check : 'a mt -> bool
type side = L | R
val check_is_at : h -> h list -> side list -> 'a  -> bool
val build_node : 'a mt -> 'a mt -> 'a mt
val insert : 'a -> 'a mt -> 'a mt
val build : 'a list -> 'a mt
val depth : 'a mt -> int
val size : 'a mt -> int
val leaf_count : 'a mt -> int