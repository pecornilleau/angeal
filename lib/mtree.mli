type h 
val ( ^ ) : h -> h -> h
type 'a mt = L of 'a |  N of h * 'a mt * 'a mt
val print_hash : h -> string
val hash : 'b -> h
val bhash : 'a mt -> 'a mt -> h
val get_hash : 'a mt -> h
val check : 'a mt -> bool
val build_node : 'a mt -> 'a mt -> 'a mt
val insert : 'a -> 'a mt -> 'a mt
val build : 'a list -> 'a mt
val depth : 'a mt -> int
val size : 'a mt -> int
val leaf_count : 'a mt -> int