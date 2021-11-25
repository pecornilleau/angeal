module type Hash_Sig =
sig
    type h
    val hash : 'a -> h
    val (^) : h -> h -> h
    val print_hash : h -> string
end

module Trivial_Hash : Hash_Sig =
struct
    type h = string
    let (^) a b = a^b 
    let hash v= Printf.sprintf "%010i" (Hashtbl.hash v)
    let print_hash h = Printf.sprintf "%s" h
    let%test "hash size" =
    (String.length (hash 0)) = 10
    let%test "hash size 2" =
    (String.length (hash "ceci est une chaine un peu plus longue")) = 10
end
module Make = functor (MyHash : Hash_Sig) ->
struct
    include MyHash

    type 'a mt = Leaf of 'a |  Node of h * 'a mt * 'a mt


    let dhash h1 h2 = hash (h1^h2)


    (* **************************** *)
    (* hash manipulation *)
    let get_hash = function
        | Leaf i -> hash i
        | Node (h,_,_) -> h

    let dhash_tree t1 t2 = dhash (get_hash t1) (get_hash t2)

    (* **************************** *)
    (* tree building *)
    let build_node t1 t2 =
        Node(dhash_tree t1 t2, t1,t2)

    let%test "build" = 
        (get_hash (build_node (Leaf 1) (Leaf 2))) = (hash ((hash 1)^(hash 2)))

    (* Random insertion : not quite balance but close enough *)
    module Rand =
    struct
        (* heads insert to the left, tails insert to the right *)
        let r_goleft () = Random.self_init (); Random.int 100 < 50

        let rec insert v t = match t with
            | Leaf i -> build_node (Leaf v) (Leaf i)
            | Node(_,t1,t2) -> 
                if r_goleft ()
                then 
                    let t1' = insert v t1 in
                    build_node t1' t2
                else 
                    let t2' = insert v t2 in
                    build_node t1 t2'

        let rec build = function
            | [] -> raise (Failure "no empty merkle")
            | [i] -> Leaf i
            | i::q -> 
                let t = build q in
                insert i t 
    end
    include Rand







    (* **************************** *)
    (* binary tree operations *)
    let rec depth = function
        | Leaf _ -> 1
        | Node(_,t1,t2) -> 1 + max (depth t1) (depth t2)

    let%test "depth" =
        3 = (depth (build_node (Leaf 1) (build_node (Leaf 2) (Leaf 3))))

    let rec leaf_count = function
        | Leaf _ -> 1
        | Node (_,t1,t2) -> (leaf_count t1) + (leaf_count t2)

    let%test "leaf_count" =
        3 = (leaf_count (build_node (Leaf 1) (build_node (Leaf 2) (Leaf 3))))

    let rec size = function
        | Leaf _ -> 1
        | Node(_,t1,t2) -> 1 + (size t1) + (size t2)
        
    let%test "size" =
        5 = (size (build_node (Leaf 1) (build_node (Leaf 2) (Leaf 3))))



    (* **************************** *)
    (* checking *)
    let rec check tree = match tree with
        | Leaf _ -> true
        | Node (h,t1,t2) -> h = dhash_tree t1 t2 && check t1 && check t2


    type side = L | R

    (** 
    check_is_at root hashes trail data
    - hashes is the list of hashes needed on the way back to the root 
    - trail is the inverted list of the choices to make to reach data
    *)  
    let check_is_at ?(debug=false) root hashes trail data =
        let rec follow hashes trail last_h = match (hashes,trail) with
            | h::hashes',c::trail' ->
                begin
                    match c with
                        | L -> 
                            (* we come from the left *)
                            if debug then Printf.printf "(%s) %s\n" (print_hash last_h) (print_hash  h);
                            follow hashes' trail' (dhash last_h h)
                        | R -> 
                            if debug then Printf.printf "%s (%s)\n" (print_hash h) (print_hash last_h);
                            follow hashes' trail' (dhash h last_h)
                end
            | [],[] -> last_h
            | _ -> raise (Failure "inconsistent nb of hashes for given trail")
        in
        let computed_root = follow hashes trail (hash data) in
        if debug then Printf.printf "(computed root %s)\n" (print_hash computed_root);
        if debug then Printf.printf "root %s\n" (print_hash root);
        computed_root = root

    let rec search value tree =
        match tree with
            | Leaf v -> v=value,[]
            | Node (_,l,r) -> 
                let found_l,trail_l = search value l in
                if found_l 
                    then (true,L::trail_l)
                else
                let found_r,trail_r = search value r in
                if found_r 
                    then (true,R::trail_r)
                else (false,[])    

    let get_proof value tree = 
        let rec reverse_proof value tree = 
        match tree with
            | Leaf v -> v=value,[],[]
            | Node (_,l,r) -> 
                let found,hashes,trail = reverse_proof value l in
                if found 
                    then true,get_hash r::hashes,L::trail
                else
                let found,hashes,trail = reverse_proof value r in
                if found
                    then true,get_hash l::hashes,R::trail
                else false,[],[]
        in
        let found,hashes,trail = reverse_proof value tree in
        found, List.rev hashes, List.rev trail

end