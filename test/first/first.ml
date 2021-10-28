open Angeal
open Mtree_first

let tree_s_ok = Nu (hash (L 1), L 1)
let tree_d_ok = N (hash tree_s_ok + hash (L 1), tree_s_ok, L 1)
let test_ok t () = Alcotest.(check bool) "" true (check t) 

let tree_ko = Nu (hash (L 1), L 2)
let test_ko () = Alcotest.(check bool) "" false (check tree_ko) 


let rec upto n = if n == 0 then [0] else n::(upto (n-1))

(* Run it *)
let () =
  let open Alcotest in
  run "check" [
      "simple", [
          test_case ""     `Quick (test_ok tree_s_ok);
          test_case ""     `Quick (test_ok tree_d_ok);
          test_case ""     `Quick test_ko;
        ];
      "random", [
          test_case ""     `Quick (test_ok (r_build (upto 16)));
          test_case ""     `Quick (test_ok (r_build (upto 16)));
          test_case ""     `Quick (test_ok (r_build (upto 16)));
          test_case ""     `Quick (test_ok (r_build (upto 16)));
        ];
    ]