type h = string
let hash v= Printf.sprintf "%010i" (Hashtbl.hash v)

let bhash a b = hash (a ^ b)



type 'a mt = L of 'a |  N of h * 'a mt * 'a mt

let get_hash = function
    | L i -> hash i
    | N (h,_,_) -> h

let bhash t1 t2 = bhash (get_hash t1) (get_hash t2)

let build t1 t2 =
    N(bhash t1 t2, t1,t2)
    
let get_hash t = match t with
    | L i -> hash i
    | N (h,_,_) -> h

let rec check tree = match tree with
    | L _ -> true
    | N (h,t1,t2) -> h == bhash t1 t2 && check t1 && check t2

let goleft () = Random.self_init (); Random.int 100 < 50


let rec r_insert v t = match t with
    | L i -> build (L v) (L i)
    | N (_,L i,t2) -> 
        let t1 = build (L v) (L i) in
        build t1 t2
    | N(_,t1, L i) -> 
        let t2 = build (L v) (L i) in
        build t1 t2
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
        