signature KERNELCONSTRAINTS =
  sig
    type atype
    type ('a,'b) annexp
    type 'a variable
    type 'a bintree
    val foreach : ('a -> unit) -> 'a bintree -> unit
    type expatt
    type varatt
    val Cexp : (expatt,varatt) annexp
               -> (atype * atype) bintree * atype list * atype
                  * varatt variable list
  end


