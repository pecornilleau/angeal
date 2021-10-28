
type h = string
let (^) a b = a^b 
let hash v= Printf.sprintf "%010i" (Hashtbl.hash v)
let print_hash h = Printf.sprintf "%s" h

let%test "hash size" =
  (String.length (hash 0)) = 10
let%test "hash size 2" =
  (String.length (hash "ceci est une chaine un peu plus longue")) = 10




type 'a mt = L of 'a |  N of h * 'a mt * 'a mt

let get_hash = function
    | L i -> hash i
    | N (h,_,_) -> h

let bhash t1 t2 = hash ((get_hash t1)^(get_hash t2))

let build t1 t2 =
    N(bhash t1 t2, t1,t2)

let%test "build" = 
    (get_hash (build (L 1) (L 2))) = (hash ((hash 1)^(hash 2)))


let rec check tree = match tree with
    | L _ -> true
    | N (h,t1,t2) -> h = bhash t1 t2 && check t1 && check t2

let goleft () = Random.self_init (); Random.int 100 < 50


let rec r_insert v t = match t with
    | L i -> build (L v) (L i)
    | N(_,t1,t2) -> 
        if goleft ()
        then 
            let t1' = r_insert v t1 in
            build t1' t2
        else 
            let t2' = r_insert v t2 in
            build t1 t2'

let rec r_build = function
    | [] -> raise (Failure "no empty merkle")
    | [i] -> L i
    | i::q -> 
        let t = r_build q in
        r_insert i t 







let rec depth = function
    | L _ -> 1
    | N(_,t1,t2) -> 1 + max (depth t1) (depth t2)

let%test "depth" =
    3 = (depth (build (L 1) (build (L 2) (L 3))))

let rec leaf_nb = function
    | L _ -> 1
    | N (_,t1,t2) -> (leaf_nb t1) + (leaf_nb t2)

let%test "leaf_nb" =
    3 = (leaf_nb (build (L 1) (build (L 2) (L 3))))

let rec size = function
    | L _ -> 1
    | N(_,t1,t2) -> 1 + (size t1) + (size t2)
    
let%test "size" =
    5 = (size (build (L 1) (build (L 2) (L 3))))

