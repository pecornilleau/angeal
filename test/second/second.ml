open Angeal
open Mtree

let check_ok t () = Alcotest.(check bool) "" true (check t) 
let check_ko t () = Alcotest.(check bool) "" false (check t) 

let rec upto n = if n == 1 then [1] else n::(upto (n-1))

let t12 = build_node (Leaf 1) (Leaf 2)
let t34 = build_node (Leaf 3) (Leaf 4)
let t56 = build_node (Leaf 5) (Leaf 6)
let t78 = build_node (Leaf 7) (Leaf 8)

let t14 = build_node t12 t34
let t58 = build_node t56 t78
let tree = build_node t14 t58
(* 
       ^
   ^       ^
 ^   ^   ^   ^
1 2 3 4 5 6 7 8 
*)
let root = get_hash tree

let check_is_at_ok  v trail hashes () =  Alcotest.(check bool) "" true (check_is_at root hashes trail v) 
let check_is_at_ko  v trail hashes () =  Alcotest.(check bool) "" false (check_is_at root hashes trail v) 

(* Run it *)
let () =
  let open Alcotest in
  run "basic" [
      "check", [
          test_case ""     `Quick (check_ok (Leaf 1));
          test_case ""     `Quick (check_ok (Node (hash ((hash 1)^(hash 2)),Leaf 1,Leaf 2) ));          
          test_case ""     `Quick (check_ok tree);

          (* wrong leaves *)
          test_case ""     `Quick (check_ko (Node (hash ((hash 1)^(hash 2)),Leaf 3,Leaf 2) ));
          test_case ""     `Quick (check_ko (Node (hash ((hash 1)^(hash 2)),Leaf 1,Leaf 3) ));
          test_case ""     `Quick (check_ko (Node (hash ((hash 1)^(hash 2)),Leaf 2,Leaf 1) ));

          (* wrong hash technique *)
          test_case ""     `Quick (check_ko (Node ((hash 1)^(hash 2),Leaf 1,Leaf 2) ));
        ];
      "random", [
          test_case ""     `Quick (check_ok (build (upto 16)));
          test_case ""     `Quick (check_ok (build (upto 32)));
          test_case ""     `Quick (check_ok (build (upto 1024)));
          test_case ""     `Quick (check_ok (build (upto 16384)));
      ];
      "is at", [
          test_case ""     `Quick (check_is_at_ok 1 [L;L;L] [hash 2;get_hash t34;get_hash t58]);
          test_case ""     `Quick (check_is_at_ok 8 [R;R;R] [hash 7;get_hash t56;get_hash t14]);
          test_case ""     `Quick (check_is_at_ok 5 [L;L;R] [hash 6;get_hash t78;get_hash t14]);
          test_case ""     `Quick (check_is_at_ok 3 [L;R;L] [hash 4;get_hash t12;get_hash t58]);

          (* wrong data*)
          test_case ""     `Quick (check_is_at_ko 2 [L;L;L] [hash 2;get_hash t34;get_hash t58]);
          (* wrong hash trail *)
          test_case ""     `Quick (check_is_at_ko 1 [L;L;L] [hash 2;get_hash t12;get_hash t58]);
          test_case ""     `Quick (check_is_at_ko 1 [L;L;L] [hash 42;get_hash t34;get_hash t58]);
          (* wrong trail *)
          test_case ""     `Quick (check_is_at_ko 1 [L;R;L] [hash 2;get_hash t34;get_hash t58]);
      ];
    ];