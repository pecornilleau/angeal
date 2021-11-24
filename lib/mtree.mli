(* hash definition *)
type h 
val ( ^ ) : h -> h -> h
val print_hash : h -> string
val hash : 'b -> h

(* tree definition *)
type 'a mt = Leaf of 'a |  Node of h * 'a mt * 'a mt
val build_node : 'a mt -> 'a mt -> 'a mt
val insert : 'a -> 'a mt -> 'a mt
val build : 'a list -> 'a mt
val get_hash : 'a mt -> h

(* checking operations *)
val check : 'a mt -> bool
type side = L | R
val check_is_at : ?debug:bool ->h -> h list -> side list -> 'a  -> bool

(* Binary tree operations *)
val depth : 'a mt -> int
val size : 'a mt -> int
val leaf_count : 'a mt -> int