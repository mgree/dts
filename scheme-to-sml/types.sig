signature KERNELTYPES =
  sig
    datatype type_tag = FUNC | BOOL | NIL | PAIR | UNSPEC
    type attributes
    type atype
    datatype utype =
      TVAR of int
    | SIMPLE of type_tag * atype list
    | DYN of type_tag -> atype

    val utype : atype -> utype
    val new_typevar : unit -> atype
    val make_type : type_tag * atype list -> atype
    val equiv : atype * atype -> unit
    val unify : atype * atype -> unit
    val pred_succ : atype * atype -> unit
    val propagate : atype list * atype list -> unit
    val interpret : atype -> unit
  end
