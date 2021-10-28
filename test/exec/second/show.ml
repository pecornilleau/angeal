open Angeal

open Mtree
let show t = 
    Format.printf "----------------\n";
    Mtprint.pprint t;
    Format.printf "-> %b\n" (check t)

let _ = show (L 1)
let _ = show (L 2)

let tree_s_ok = build (L 1) (L 2)
let _ = show tree_s_ok

let tree_d_ok = build tree_s_ok (L 1)
let _ = show tree_d_ok

let tree_d_ok2 = build tree_s_ok (L 2)
let _ = show tree_d_ok2

let _ = show  (build tree_d_ok tree_d_ok2)


let _ = show (r_build [1;2;3;4])
let _ = show (r_build [1;2;3;4])
let _ = show (r_build [1;2;3;4])

let rec upto n = if n == 0 then [0] else n::(upto (n-1))
    
let _ = show (r_build (upto 16))
let _ = show (r_build (upto 16))
let _ = show (r_build (upto 16))
let _ = show (r_build (upto 16))
(*let _ = show (r_build (upto 16))
let _ = show (r_build (upto 16))
let _ = show (r_build (upto 16)) *)

let _ = Printf.printf "%010i" 105