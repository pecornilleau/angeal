open Angeal

module MyTree = Mtree.Make (Mtree.Trivial_Hash)
open MyTree

let show ?(b = true) t = 
    Format.printf "----------------\n";
    if b then Mtprint.pprint t;
    Format.printf "check -> %b\n" (check t);
    Format.printf "depth -> %i\n" (depth t);
    Format.printf "size -> %i\n" (size t);
    Format.printf "leafs -> %i\n" (leaf_count t)
let _ = show (Leaf 1)
let _ = show (Leaf 2)

let tree_s_ok = build_node (Leaf 1) (Leaf 2)
let _ = show tree_s_ok

let tree_d_ok = build_node tree_s_ok (Leaf 1)
let _ = show tree_d_ok

let tree_d_ok2 = build_node tree_s_ok (Leaf 2)
let _ = show tree_d_ok2

let _ = show  (build_node tree_d_ok tree_d_ok2)


let _ = show (build [1;2;3;4])
let _ = show (build [1;2;3;4])
let _ = show (build [1;2;3;4])

let rec upto n = if n == 1 then [1] else n::(upto (n-1))
    
let _ = Format.print_string "---- 2^4 leafs ------"
let _ = show  (build (upto 16))
let _ = show ~b:false (build (upto 16))
let _ = show ~b:false (build (upto 16))
let _ = Format.print_string "---- 2^5 leafs ------"
let _ = show ~b:false (build (upto 32)) 
let _ = show ~b:false (build (upto 32)) 
let _ = Format.print_string "---- 2^6 leafs ------"
let _ = show ~b:false (build (upto 64)) 
let _ = show ~b:false (build (upto 64)) 
let _ = show ~b:false (build (upto 64)) 
let _ = Format.print_string "---- 2^8 leafs ------"
let _ = show ~b:false (build (upto 256)) 
let _ = show ~b:false (build (upto 256)) 
let _ = show ~b:false (build (upto 256)) 
let _ = Format.print_string "---- 2^10 leafs ------"
let _ = show ~b:false (build (upto 1024)) 
let _ = Format.print_string "---- 2^14 leafs ------"
let _ = show ~b:false (build (upto 16384)) 



let _ = Format.print_string "---- balanced tree 1 to 8 ------\n"

let t12 = build_node (Leaf 1) (Leaf 2)
let t34 = build_node (Leaf 3) (Leaf 4)
let t56 = build_node (Leaf 5) (Leaf 6)
let t78 = build_node (Leaf 7) (Leaf 8)

let t14 = build_node t12 t34
let t58 = build_node t56 t78
let t18 = build_node t14 t58

let _ = Mtprint.pprint t18
let _ = Format.print_string "---- check proof of 1  ------\n"

let _ = check_is_at ~debug:true (get_hash t18)  [hash 2;get_hash t34;get_hash t58]  [L;L;L] 1