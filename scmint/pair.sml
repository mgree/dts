(*  structure Pair: PAIR =
    struct
    let open Object in *)

    fun cons p = p
    fun car (v1,v2) = v1
    fun cdr (v1,v2) = v2
    fun set_car ((v1,v2),v3) = v1 := !v3
    fun set_cdr ((v1,v2),v3) = v2 := !v3

    val car_dyn = car o PAIR_UNTAG
    val cdr_dyn = cdr o PAIR_UNTAG

    fun caar (v1,v2) = car_dyn v1
    fun cadr (v1,v2) = car_dyn v2
    fun cdar (v1,v2) = cdr_dyn v1
    fun cddr (v1,v2) = cdr_dyn v2
    fun caaar (v1,v2) = car_dyn (car_dyn v1)
    fun caadr (v1,v2) = car_dyn (car_dyn v2)
    fun cadar (v1,v2) = car_dyn (car_dyn v1)
    fun caddr (v1,v2) = car_dyn (car_dyn v2)
    fun cdaar (v1,v2) = cdr_dyn (car_dyn v1)
    fun cdadr (v1,v2) = cdr_dyn (car_dyn v2)
    fun cddar (v1,v2) = cdr_dyn (car_dyn v1)
    fun cdddr (v1,v2) = cdr_dyn (car_dyn v2)
    fun caaaar (v1,v2) = car_dyn (caaar (v1,v2))
    fun caaadr (v1,v2) = car_dyn (caadr (v1,v2))
    fun caadar (v1,v2) = car_dyn (cadar (v1,v2))
    fun caaddr (v1,v2) = car_dyn (caddr (v1,v2))
    fun cadaar (v1,v2) = car_dyn (cdaar (v1,v2))
    fun cadadr (v1,v2) = car_dyn (cdadr (v1,v2))
    fun caddar (v1,v2) = car_dyn (cddar (v1,v2))
    fun cadddr (v1,v2) = car_dyn (cdddr (v1,v2))
    fun cdaaar (v1,v2) = cdr_dyn (caaar (v1,v2))
    fun cdaadr (v1,v2) = cdr_dyn (caadr (v1,v2))
    fun cdadar (v1,v2) = cdr_dyn (cadar (v1,v2))
    fun cdaddr (v1,v2) = cdr_dyn (caddr (v1,v2))
    fun cddaar (v1,v2) = cdr_dyn (cdaar (v1,v2))
    fun cddadr (v1,v2) = cdr_dyn (cdadr (v1,v2))
    fun cdddar (v1,v2) = cdr_dyn (cddar (v1,v2))
    fun cddddr (v1,v2) = cdr_dyn (cdddr (v1,v2))
(*
    end
    end
*)