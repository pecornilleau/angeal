open Mtree_first
open Format
let pprint t = 
    let rec loop n t =
        let spaces = String.make (2*n) ' ' in
        match t with 
            | L i ->        printf "%s%d\n" spaces i
            | Nu (_,t') ->  printf "%s%c " spaces '-'; 
                            loop 0  t';
                            (* printf "%s%c\n" spaces '/' *)
            | N(_,t1,t2) ->
                            loop (n+1)  t1;
                            printf "%s%c\n" spaces '<'; 
                            loop (n+1)  t2
    in
    loop 0 t    