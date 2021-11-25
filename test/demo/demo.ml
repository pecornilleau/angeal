open Format
open Angeal

(* just a simple list builder for demo purpose *)
let rec upto n = if n == 1 then [1] else n::(upto (n-1))

(* To build a Mtree module, you need an input module of type Hash_Sig, to define a hash function.
    A trivial example is given by Trivial_Hash.*)
(* Just apply the Make functor *)
module Merkle = Mtree.Make (Mtree.Trivial_Hash)

(* to build a tree, use "build" *)
let tree = Merkle.build (upto 6)
let _ = printf "--------------------\n"
let _ = printf "A tree of 6 elements\n"
let _ = Mtprint.pprint tree
let _ = printf "root : %s\n" (Merkle.print_hash (Merkle.get_hash tree))

(* You can check a tree is well formed (hashes are good) *)
let _ = printf "--------------------\n"
let is_wf = Merkle.check tree
let _ = printf "is it well formed : %b\n" is_wf

(* You can insert something in it *)
let _ = printf "--------------------\n"
let _ = printf "let's add 11\n"
let tree2 = Merkle.insert 11 tree
let _ = Mtprint.pprint tree2
let root2 = Merkle.get_hash tree2
let _ = printf "new root : %s\n" (Merkle.print_hash root2)
let _ = printf "--------------------\n"
let _ = printf "is it still well formed : %b\n" (Merkle.check tree2)

(* you can find a value in the tree *)
let found,trail = Merkle.search 11 tree2
let _ = printf "To get to 11 go : %s\n" (Mtprint.print_trail trail)



(* you can get a proof that a value is in the tree *)
let _,hashes,trail = Merkle.get_proof 11 tree2
let _ = printf "proof : %s\n" (Mtprint.print_proof hashes trail)
let _ = printf "a proof start at the value, and goes back to the root.\nAt each step, we have the hash needed for the step, and the side the value can be found at (looking from the root)\n" 


(* and check that proof against the hash of a root *)
let _ = printf "--------------------\n"
let _ = printf "let's check the proof\n"
let _ = printf "values in () are computed from known values\n"
let _ = Merkle.check_is_at ~debug:true root2 hashes trail 11