open Angeal

open Mtree
let show ?(b = true) t = 
    Format.printf "----------------\n";
    if b then Mtprint.pprint t;
    Format.printf "check -> %b\n" (check t);
    Format.printf "depth -> %i\n" (depth t);
    Format.printf "size -> %i\n" (size t)
let _ = show (L 1)
let _ = show (L 2)

let tree_s_ok = build_node (L 1) (L 2)
let _ = show tree_s_ok

let tree_d_ok = build_node tree_s_ok (L 1)
let _ = show tree_d_ok

let tree_d_ok2 = build_node tree_s_ok (L 2)
let _ = show tree_d_ok2

let _ = show  (build_node tree_d_ok tree_d_ok2)


let _ = show (build [1;2;3;4])
let _ = show (build [1;2;3;4])
let _ = show (build [1;2;3;4])

let rec upto n = if n == 0 then [0] else n::(upto (n-1))
    
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
let _ = show ~b:false (build (upto 1024)) 
let _ = show ~b:false (build (upto 1024)) 
let _ = Printf.printf "%010i" 105