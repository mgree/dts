(* types and all *)

structure KernelTypes =
  struct
  local open SchemeGeneral UnionFind
  in

  datatype type_tags = FUNC | BOOL | NIL | PAIR

  datatype utype =
    VAR of int
  | SIMPLE of type_tags * atype list
  | DYN of atype list
  and attributes = 
    ATT of { equivptr: atype Option ref, 
             preds: atype list ref, succs: atype list ref,
             pos: bool ref, neg: bool ref }
  withtype atype = (utype * attributes) UF

  fun make_attrib () = 
      ATT { equivptr = ref None,
            preds = ref nil, succs = ref nil,
            pos = ref false, neg = ref false }

  fun attributes aty = (case !!aty of (_, ATT atts) => atts)

  fun get_preds aty  =  #preds (attributes aty)

  fun get_succs aty = #succs (attributes aty)

  fun get_equivptr aty = #equivptr (attributes aty)

  (* make a new type variable *)
  local 
     val counter = ref 0 
  in
  fun new_typevar () = 
      (counter := !counter + 1; 
       make (VAR (!counter), make_attrib()))
  end

  (* make a simple type with suitable dynamic equiv representative *)
  fun make_type (stype as (FUNC, [_, _])) = 
      let val r = make (SIMPLE stype, make_attrib())
          val rd = make (DYN [r, new_typevar(), new_typevar(), new_typevar()], make_attrib())
      in (get_equivptr r := Some rd; r)
      end
    | make_type (stype as (FUNC, _)) =
      raise IllegalInput ("Wrong number of arguments to function type constructor", "")
    | make_type (stype as (BOOL, [])) = 
      let val r = make (SIMPLE stype, make_attrib())
          val rd = make (DYN [new_typevar(), r, new_typevar(), new_typevar()], make_attrib())
      in (get_equivptr r := Some rd; r)
      end
    | make_type (stype as (BOOL, _)) =
      raise IllegalInput ("Wrong number of arguments to Bool type constructor", "")
    | make_type (stype as (PAIR, [_, _])) = 
      let val r = make (SIMPLE stype, make_attrib())
          val rd = make (DYN [new_typevar(), new_typevar(), r, new_typevar()], make_attrib())
      in (get_equivptr r := Some rd; r)
      end
    | make_type (stype as (PAIR, _)) =
      raise IllegalInput ("Wrong number of arguments to pair type constructor", "")
    | make_type (stype as (NIL, [])) = 
      let val r = make (SIMPLE stype, make_attrib())
          val rd = make (DYN [new_typevar(), new_typevar(), new_typevar(), r], make_attrib())
      in (get_equivptr r := Some rd; r)
      end
    | make_type (stype as (NIL, _)) =
      raise IllegalInput ("Wrong number of arguments to nil type constructor", "")

  (* find equiv representative of annotated type *)
  fun ecr t =
      (case !(get_equivptr t) of
         None => t
       | Some t' => ecr t')

  fun zip f ([], []) = ()
    | zip f (a::r, a'::r') = (f (a, a'); zip f (r, r'))
    | zip f (_, _) = raise IllegalInput ("Lists of unequal lengths in zip", "")

  fun equiv (t1: atype, t2: atype) =
      let val t1' = ecr t1
          val t2' = ecr t2
      in if equal(t1', t2')
            then ()
         else (case (#1 (!!t1'), #1 (!!t2')) of
                (DYN stl, DYN stl') => (union #2 (t1', t2'); zip alias (stl, stl'))                     
              | (VAR _, DYN _) => get_equivptr t1' := Some t2'
              | (DYN _, VAR _) => get_equivptr t2' := Some t1'
              | (VAR _, VAR _) => get_equivptr t1' := Some t2'
              | (_,_) => raise IllegalInput ("Illegal type equiving attempted in equiv", ""))
      end
  and alias (t1, t2) =
      if equal (t1, t2) 
         then ()
      else (case (#1 (!!t1), #1 (!!t2)) of
             (SIMPLE (tt, tlist), SIMPLE (tt', tlist')) =>
                if tt = tt' then
                   (union #2 (t1, t2);
                    equiv (t1, t2); (* <-- this is probably unnecessary *)
                    zip alias (tlist, tlist'))
                else raise IllegalInput ("Equiv error: different tags encountered", "")
           | (VAR _, SIMPLE _) => union #2 (t1, t2)
           | (SIMPLE _, VAR _) => union #1 (t1, t2)
           | (VAR _, VAR _) => union #2 (t1, t2)
           | (_,_) => raise IllegalInput ("Illegal type aliasing attempted in alias", "") 
           )
  end

  fun unify (t1, t2) =
      if equal (t1, t2) 
         then ()
      else (case (#1 (!!t1), #1 (!!t2)) of
              (DYN stl, DYN stl') => (union #2 (t1, t2); zip unify (stl, stl'))
            | (SIMPLE (tt, tlist), SIMPLE (tt', tlist')) =>
                 if tt = tt' then
                    (union #2 (t1, t2); zip unify (tlist, tlist'))
                 else raise IllegalInput ("Functor clash in unify", "")
            | (VAR _, _) => union #2 (t1, t2)
            | (_, VAR _) => union #1 (t1, t2)
            | (_, _) => raise IllegalInput ("Dyn and simple types cannot be unified", ""))
end
 