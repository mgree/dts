(* types and all *)

structure KernelTypes =
struct
local open SchemeGeneral
in

datatype utype =
  VAR of int
| SIMPLE of type_tags * atype list
| DYN of atype list
and attributes = 
  ATT of { equivptr: atype ref, aliasptr: atype ref,
           preds: atype list ref, succs: atype list ref,
           pos: bool ref, neg: bool ref }
and type_tags = FUNC | BOOL | NIL | PAIR
withtype atype = (utype * attributes) Option ref

fun make_attrib () = 
    ATT { equivptr = ref (ref None), aliasptr = ref (ref None),
          preds = ref nil, succs = ref nil,
          pos = ref false, neg = ref false }

fun attributes (ref (Some (_, ATT atts))) = atts
  | attributes _ = raise IllegalInput 
        ("Input not a valid atype in a attributes", "")
fun get_preds aty  =  (#preds)(attributes aty)

fun get_succs aty = (#succs)(attributes aty)

local 
   val counter = ref 0 
in
fun new_typevar () = 
    (counter := !counter + 1; 
     ref (Some (VAR (!counter), make_attrib())))
end

fun make_st stype =
    ref (Some (SIMPLE stype, make_attrib()))

fun make_dyn_type (stype as (FUNC, ts)) =
    ref (Some (DYN [make_st stype, new_typevar(), new_typevar(), new_typevar()], make_attrib()))
  | make_dyn_type (stype as (BOOL, ts)) =
    ref (Some (DYN [new_typevar(), make_st stype, new_typevar(), new_typevar()], make_attrib()))
  | make_dyn_type (stype as (NIL, ts)) =
    ref (Some (DYN [new_typevar(), new_typevar(), make_st stype, new_typevar()], make_attrib()))
  | make_dyn_type (stype as (PAIR, ts)) =
    ref (Some (DYN [new_typevar(), new_typevar(), new_typevar(), make_st stype], make_attrib()))

(* make a simple type with suitable dynamic equiv representative *)
fun make_type stype = 
    let val r = ref (Some (SIMPLE stype, make_attrib()))
    in (#equivptr (attributes r) := make_dyn_type stype; r)
    end

(* find representative of annotated type *)
fun find accfcn (t as ref (Some (_, ATT atts))) =
    let val atref = accfcn atts
    in (case !atref of
       (ref None) => t
     | t' => let val t'' = find accfcn t' 
             in (atref := t''; t'')
             end)
    end
  | find accfcn _ = raise IllegalInput ("None atype in find -- impossible here", "")

(* union two types; assume the two types are distinct legal representatives *) 
fun union accfcn t1 (ref (Some (VAR _, ATT atts))) = accfcn atts := t1
  | union accfcn (ref (Some (_, ATT atts))) t2 =
       accfcn atts := t2
  | union accfcn _ _ = raise IllegalInput ("Invalid atype representatives in union", "")

fun zip f [] [] = ()
  | zip f (a::r) (a'::r') = (f a a'; zip f r r')
  | zip f _ _ = raise IllegalInput ("Lists of unequal lengths in zip", "")

fun equiv (t1: atype) (t2: atype) =
    let val t1' = find #equivptr t1
        val t2' = find #equivptr t2
    in if t1' = t2' then ()
           else union #equivptr t1' t2';
                case (!t1', !t2') of
                   (Some (DYN stl, _), Some (DYN stl', _)) => zip alias stl stl'
                 | (Some (SIMPLE (tt, tlist), _), Some (SIMPLE (tt', tlist'), _)) =>
                       if tt = tt' then
                          zip alias tlist tlist'
                       else raise IllegalInput ("Equiv error: different tags encountered", "")
                 | (Some (VAR _, _), _) => ()
                 | (_, Some (VAR _, _)) => ()
                 | (_,_) => raise IllegalInput ("Illegal type equiving attempted in equiv", "")
    end
and alias t1 t2 =
    let val t1' = find #aliasptr t1
        val t2' = find #aliasptr t2
    in if t1' = t2' then ()
          else (union #aliasptr t1' t2'; equiv t1' t2')
    end

end
end
