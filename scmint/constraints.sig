

signature KERNELCONSTRAINTS =
sig

    type atype
    type ('a, 'b) annexp
    type 'a anndatum 
    type 'a aotree
    type 'a bintree  
    type 'a variable  

    val Annast : unit -> (atype * atype, atype) annexp
    val Cdatum : (atype * atype) anndatum -> 
                 (atype * atype) bintree * 'a aotree * atype
    val Cexp : (atype * atype, atype) annexp ->
               (atype * atype) bintree * atype variable aotree * atype
    val init : unit -> atype * atype
 
    val foreach: ('a -> unit) -> 'a bintree -> unit

    val free_var_occs: atype variable aotree -> atype variable list

    val unused_formals: 'a variable aotree -> 'a variable list

end


