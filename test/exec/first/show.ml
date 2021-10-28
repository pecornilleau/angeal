open Angeal

open Mtree_first
let show t = 
    Format.printf "----------------\n";
    Mtprint.pprint t;
    Format.printf "-> %b\n" (check t)

let tree_s_ok = Nu (hash (L 1), L 1)
let tree_d_ok = N (hash tree_s_ok + hash (L 1), tree_s_ok, L 1)

let tree_d_ok2 = N (hash tree_s_ok + hash (L 2), tree_s_ok, L 2)

let _ = show  (build tree_d_ok tree_d_ok2)


let _ = show (r_build [1;2;3;4])
let _ = show (r_build [1;2;3;4])
let _ = show (r_build [1;2;3;4])

let rec upto n = if n == 0 then [0] else n::(upto (n-1))
    
let _ = show (r_build (upto 16))
let _ = show (r_build (upto 16))
let _ = show (r_build (upto 16))
let _ = show (r_build (upto 16))

let _ = Printf.printf "%010i" 105