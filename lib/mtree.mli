(** Signature of minimum hash module *)
module type Hash_Sig =
sig
    type h
    val hash : 'a -> h
    val (^) : h -> h -> h
    val print_hash : h -> string
end

(** Example of a super simple Hash module, based on Hashtbl.hash *)
module Trivial_Hash : Hash_Sig

module Make : functor (MyHash : Hash_Sig) -> 
sig
    (* hash definition *)
    type h 
    val ( ^ ) : h -> h -> h
    val print_hash : h -> string
    val hash : 'b -> h

    (** tree definition *)
    type 'a mt = 
        | Leaf of 'a (** Leaves contains data *)
        | Node of h * 'a mt * 'a mt (** Internal nodes store hash  *)

    (* tree building *)
    val build_node : 'a mt -> 'a mt -> 'a mt
    val insert : 'a -> 'a mt -> 'a mt
    val build : 'a list -> 'a mt
    (** return root hash (or hash of value on leaves) *)
    val get_hash : 'a mt -> h

    (* checking operations *)
    (** is well formed *)
    val check : 'a mt -> bool

    (** Left or Right *)
    type side = L | R
    (** check_is_at root hashes trail value 
    
    checks a proof (hashes + trail) that a given value belongs to a tree with a given root     
    the trail and hashes correspond to a path to the root in the tree, starting at the value.
    *)
    val check_is_at : ?debug:bool -> h -> h list -> side list -> 'a  -> bool

    (** search for a value in a tree, and returns the path to it starting from the root *)
    val search : 'a -> 'a mt -> bool * side list

    (** search for a value in a tree, and returns a proof (hashes + trail, starting at value, back to the root) *)
    val get_proof : 'a -> 'a mt -> bool * h list * side list

    (* Binary tree operations *)
    val depth : 'a mt -> int
    val size : 'a mt -> int
    val leaf_count : 'a mt -> int
end
