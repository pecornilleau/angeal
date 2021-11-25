open Format
module MyTree = Mtree.Make (Mtree.Trivial_Hash)

    
let pprint t = 
    let rec loop n t =
        let spaces = String.make (2*n) ' ' in
        match t with 
            | MyTree.Leaf i ->        printf "%s%d\n" spaces i
            | MyTree.Node(_,t1,t2) ->
                            loop (n+1)  t1;
                            printf "%s%c\n" spaces '<'; 
                            loop (n+1)  t2
    in
    loop 0 t   

let print_side = function  
    | MyTree.L ->  "L"
    | MyTree.R ->  "R"

let rec print_trail = function 
    | [] -> ""
    | t::[] -> print_side t
    | t::q -> sprintf "%s ; %s" (print_side t) (print_trail q)

let rec print_hashes = function
    | [] -> ""
    | t::[] -> MyTree.print_hash t
    | t::q -> sprintf "%s ; %s" (MyTree.print_hash t) (print_hashes q)

let rec print_proof hashes trail = match hashes,trail with
    | [],[] -> ""
    | h::qh,t::qt -> sprintf "%s:%s %s" (print_side t) (MyTree.print_hash h) (print_proof qh qt)
    | _ -> raise (Failure "not a well formed proof")