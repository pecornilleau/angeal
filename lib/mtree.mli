module type Hash_Sig =
sig
    type h
    val hash : 'a -> h
    val (^) : h -> h -> h
    val print_hash : h -> string
end

module Trivial_Hash : Hash_Sig

module Make : functor (MyHash : Hash_Sig) -> 
sig
    (* hash definition *)
    type h 
    val ( ^ ) : h -> h -> h
    val print_hash : h -> string
    val hash : 'b -> h

    (* tree definition *)
    type 'a mt = Leaf of 'a |  Node of h * 'a mt * 'a mt

    (* tree building *)
    val build_node : 'a mt -> 'a mt -> 'a mt
    val insert : 'a -> 'a mt -> 'a mt
    val build : 'a list -> 'a mt
    val get_hash : 'a mt -> h

    (* checking operations *)
    val check : 'a mt -> bool
    type side = L | R
    val check_is_at : ?debug:bool ->h -> h list -> side list -> 'a  -> bool
    val search : 'a -> 'a mt -> bool * side list
    val get_proof : 'a -> 'a mt -> bool * h list * side list

    (* Binary tree operations *)
    val depth : 'a mt -> int
    val size : 'a mt -> int
    val leaf_count : 'a mt -> int
end
