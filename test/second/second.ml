open Angeal
module MyTree = Mtree.Make (Mtree.Trivial_Hash)
open MyTree
let check_ok t () = Alcotest.(check bool) "" true (check t) 
let check_ko t () = Alcotest.(check bool) "" false (check t) 

let rec upto n = if n == 1 then [1] else n::(upto (n-1))

let t12 = build_node (Leaf 1) (Leaf 2)
let t34 = build_node (Leaf 3) (Leaf 4)
let t56 = build_node (Leaf 5) (Leaf 6)
let t78 = build_node (Leaf 7) (Leaf 8)

let t14 = build_node t12 t34
let t58 = build_node t56 t78
let t18 = build_node t14 t58
(* 
       ^
   ^       ^
 ^   ^   ^   ^
1 2 3 4 5 6 7 8 
*)
let root = get_hash t18

let check_is_at_ok  v trail hashes () =  Alcotest.(check bool) "" true (check_is_at root hashes trail v) 
let check_is_at_ko  v trail hashes () =  Alcotest.(check bool) "" false (check_is_at root hashes trail v) 
let rec list_compare l1 l2 = match l1,l2 with 
  | [],[] -> true
  |t1::q1,t2::q2 -> t1=t2 && (list_compare q1 q2)
  | _ -> false

let test_search value tree expected_trail expected_b = 
  let found,trail = search value tree in
  found = expected_b && (list_compare trail expected_trail)
  
let search_ok value tree expected_trail expected_b () = Alcotest.(check bool) "" true (test_search value tree expected_trail expected_b)

let test_get_proof value tree expected_hashes expected_trail expected_found =
  let found,hashes,trail = get_proof value tree in
  found = expected_found && (list_compare hashes expected_hashes)&& (list_compare trail expected_trail) 

let get_proof_ok value tree expected_hashes expected_trail expected_found () = Alcotest.(check bool) "" true (test_get_proof value tree expected_hashes expected_trail expected_found )


let random_test size value =
  let tree = build (upto size) in
  let _,hashes,trail = get_proof  value tree in
  check_is_at (get_hash tree) hashes trail value

let check_random ok size value () = Alcotest.(check bool) "" ok (random_test size value)  

(* Run it *)
let () =
  let open Alcotest in
  run "basic" [
      "check", [
          test_case ""     `Quick (check_ok (Leaf 1));
          test_case ""     `Quick (check_ok (Node (hash ((hash 1)^(hash 2)),Leaf 1,Leaf 2) ));          
          test_case ""     `Quick (check_ok t18);

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
          (* test_case ""     `Slow (check_ok (build (upto 1024))); *)
          (* test_case ""     `Slow (check_ok (build (upto 16384))); *)
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
      "search", [
          test_case ""     `Quick (search_ok 1 t18 [L;L;L] true);
          test_case ""     `Quick (search_ok 10 t18 [] false);
          test_case ""     `Quick (search_ok 2 t18 [L;L;R] true);
          test_case ""     `Quick (search_ok 6 t18 [R;L;R] true);

          test_case ""     `Quick (search_ok 1 (Leaf 1) [] true);
          test_case ""     `Quick (search_ok 1 (Leaf 2) [] false);
      ];
      "get_proof", [
          test_case ""     `Quick (get_proof_ok 1 (Leaf 1) [] [] true);
          test_case ""     `Quick (get_proof_ok 1 t12 [hash 2] [L] true);
          test_case ""     `Quick (get_proof_ok 2 (Leaf 1) [] [] false);

          test_case ""     `Quick (get_proof_ok 1 t18 [hash 2;get_hash t34;get_hash t58] [L;L;L] true);
          test_case ""     `Quick (get_proof_ok 10 t18 [] [] false);
          test_case ""     `Quick (get_proof_ok 3 t18 [hash 4;get_hash t12;get_hash t58] [L;R;L] true);
      ];
      "random proof", [
          test_case ""     `Quick (check_random true 10 5);
          test_case ""     `Quick (check_random true 10 1);
          test_case ""     `Quick (check_random false 10 11);
          test_case ""     `Quick (check_random true 100 55);
      ];

    ];