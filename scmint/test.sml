
(* One has  T 0 => 2  *)
fun T x =
    let val y = x
    in
        if x = 0 then 0
        else 1;
             2
    end

(*  whereas U 0 => 0 *)
fun U x =
    let val y = x 
    in
        if x = 0 then 0
        else (1;
              2)
    end 
