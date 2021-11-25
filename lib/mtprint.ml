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