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
  ATT of { equivptr: atype, aliasptr: atype }
and type_tags = FUNC | BOOL | NIL | PAIR
withtype atype = (utype * attributes) Option ref

fun make_attrib () = ATT { equivptr = ref None, aliasptr = ref None}

local 
   val counter = ref 0 
in
fun new_typevar () = 
    (counter := !counter + 1; 
     ref (Some (VAR (!counter), make_attrib())))
end

fun make_simple_type stype =
    ref (Some (SIMPLE stype, make_attrib()))

fun make_dyn_type (stype as (FUNC, ts)) =
    ref (Some (DYN [make_simple_type stype, new_typevar(), new_typevar(), new_typevar()], make_attrib()))
  | make_dyn_type (stype as (BOOL, ts)) =
    ref (Some (DYN [new_typevar(), make_simple_type stype, new_typevar(), new_typevar()], make_attrib()))
  | make_dyn_type (stype as (NIL, ts)) =
    ref (Some (DYN [new_typevar(), new_typevar(), make_simple_type stype, new_typevar()], make_attrib()))
  | make_dyn_type (stype as (PAIR, ts)) =
    ref (Some (DYN [new_typevar(), new_typevar(), new_typevar(), make_simple_type stype], make_attrib()))


(* make a simple type with suitable dynamic equiv representative *)
fun make_simple_type_with_dyn_ecr stype = 
    ref (Some (SIMPLE stype, ATT { aliasptr = ref None, equivptr = make_dyn_type stype }))

(* find representative of annotated type *)
fun find accfcn (t as ref (Some (_, ATT atts))) =
    (case accfcn atts of
       ref None => t
     | t' => let val t'' = find accfcn t' 
             in (t' := !t''; t'')
             end)
  | find accfcn _ = raise IllegalInput ("None atype in find -- impossible here", "")

(* union two types; assume the two types are distinct legal representatives *) 
fun union accfcn t1 (ref (Some (VAR _, ATT atts))) = accfcn atts := !t1
  | union accfcn (ref (Some (_, ATT atts))) t2 =
       accfcn atts := !t2
  | union accfcn _ _ = raise IllegalInput ("Invalid atype representatives in union", "")

fun zip f [] [] = ()
  | zip f (a::r) (a'::r') = (f a a'; zip f r r')
  | zip f _ _ = raise IllegalInput ("Lists of unequal lengths in zip", "")

fun equiv (t1: atype) (t2: atype) =
    let val t1' = find #equivptr t1
        val t2' = find #equivptr t2
    in if !t1' = !t2' then ()
           else union #equivptr t1' t2';
                case (!t1', !t2') of
                   (Some (DYN stl, _), Some (DYN stl', _)) => zip alias stl stl'
                 | (Some (VAR _, _), Some (DYN _, _)) => ()
                 | (Some (DYN _, _), Some (VAR _, _)) => ()
                 | (Some (VAR _, _), Some (VAR _, _)) => ()
                 | (_,_) => raise IllegalInput ("Illegal type representative in equiv", "")
    end
and alias t1 t2 =
    let val t1' = find #aliasptr t1
        val t2' = find #aliasptr t2
    in if !t1' = !t2' then ()
          else union #aliasptr t1' t2';
               (case (!t1', !t2') of
                   (Some (VAR _, _), Some (VAR _, _)) => ()
                 | (Some (VAR _, _), Some (SIMPLE _, _)) => ()
                 | (Some (SIMPLE _, _), Some (VAR _, _)) => ()
                 | (Some (SIMPLE (tt, tlist), _), Some (SIMPLE (tt', tlist'), _)) => 
                      if tt = tt' then
                         zip alias tlist tlist'
                      else raise IllegalInput ("Aliasing error: different tags encountered", "")
                 | (_, _) => raise IllegalInput ("Illegal type aliasing attempted in alias", ""));
               equiv t1' t2'
    end
end
end
