signature KERNELCONSTRAINTS =
  sig
    type atype
    type ('a,'b) annexp
    type 'a variable
    type 'a bintree
    val foreach : ('a -> unit) -> 'a bintree -> unit
    val Cexp : (atype * atype, atype) annexp
               -> (atype * atype) bintree * atype list * atype
                  * atype list
  end


