fun nothing _ = ()

val initrec = Exp.INIT { parameter = nothing, variable = nothing, exp = nothing }

fun read() = 
    let open Exp val
    in read_exp initrec std_in
    end

fun complete e =
    let open Env
    in att_exp e empty_env
    end

fun reduce (e,V,E,r) =
    let val (V',E') = simplify (V,E)
    in (e,V',E',r)
    end

