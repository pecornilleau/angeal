open Angeal
open Mtree

let check_ok t () = Alcotest.(check bool) "" true (check t) 
let check_ko t () = Alcotest.(check bool) "" false (check t) 

let rec upto n = if n == 0 then [0] else n::(upto (n-1))

(* Run it *)
let () =
  let open Alcotest in
  run "basic" [
      "check", [
          test_case ""     `Quick (check_ok (Leaf 1));
          test_case ""     `Quick (check_ok (Node (hash ((hash 1)^(hash 2)),Leaf 1,Leaf 2) ));          

          test_case ""     `Quick (check_ko (Node (hash ((hash 1)^(hash 2)),Leaf 3,Leaf 2) ));
          test_case ""     `Quick (check_ko (Node (hash ((hash 1)^(hash 2)),Leaf 1,Leaf 3) ));
          test_case ""     `Quick (check_ko (Node ((hash 1)^(hash 2),Leaf 1,Leaf 2) ));
        ];
      "random", [
          test_case ""     `Quick (check_ok (build (upto 16)));
          test_case ""     `Quick (check_ok (build (upto 16)));
          test_case ""     `Quick (check_ok (build (upto 16)));
      ];
    ];