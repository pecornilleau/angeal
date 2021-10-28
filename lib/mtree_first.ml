type h = int
let hash = Hashtbl.hash


type 'a mt = L of 'a | Nu of h * 'a mt | N of h * 'a mt * 'a mt
let build t1 t2 =
    N(hash t1 + hash t2, t1,t2)
    
let get_hash t = match t with
    | L i -> hash i
    | N (h,_,_) -> h
    | _ -> -1

let rec check tree = match tree with
    | L _ -> true
    | N (h,t1,t2) -> h == hash t1 + hash t2 && check t1 && check t2
    | Nu (h,t) -> h = hash t && check t

let goleft () = Random.self_init (); Random.int 100 < 50
let r_build t1 t2 =
    if goleft ()
    then
        build t1 t2
    else
        build t2 t1


let rec r_insert v tree = match tree with
    | L v2 -> 
        r_build (L v) (L v2)
    | Nu(_,t) -> 
        r_build (L v) t
    | N(_, t1, t2) -> 
            r_build (r_insert v t1) t2

let r_build l = 
    let rec r_build_rec t = function
        | [] -> t
        | i::q -> let t' = r_insert i t in
            r_build_rec t' q 
    in  match l with
    | [] -> L (-1)
    | i::q ->  r_build_rec (L i) q