structure KernelTypes: KERNELTYPES =
  struct
  local open SchemeGeneral UnionFind
  in

  datatype type_tag = FUNC | BOOL | NIL | PAIR | UNSPEC
  val type_tags = [FUNC, BOOL, NIL, PAIR, UNSPEC]

  datatype utype =
    TVAR of int
  | SIMPLE of type_tag * atype list
  | DYN of type_tag -> atype
  and attributes = 
    ATT of { equivptr: atype Option ref, 
             preds: atype list ref, succs: atype list ref,
             pos: bool ref, neg: bool ref, interpreted: bool ref }
  withtype atype = (utype * attributes) UF

  fun make_attrib () = 
      ATT { equivptr = ref None,
            preds = ref nil, succs = ref nil,
            pos = ref false, neg = ref false, interpreted = ref false }

  (* selection functions *)
  fun utype (aty: atype) = #1 (!!aty)
  fun attributes aty = (case !!aty of (_, ATT atts) => atts)
  fun get f aty = f (attributes aty)

  (* make a new type variable *)
  local 
     val counter = ref 0 
  in
  fun new_typevar () = 
      (counter := !counter + 1; 
       make (TVAR (!counter), make_attrib()))
  end

  (* make a new simple type *)
  fun make_type stype = make (SIMPLE stype, make_attrib())

  (* make a new dynamic type *)
  local 
  fun make_dyn_function [] = (fn _ => new_typevar())
    | make_dyn_function (a::r) =
        (case utype a of
          SIMPLE (tt, _) => (fn tt' => if tt = tt' then a else make_dyn_function r tt')
        | _ => raise IllegalInput ("Only simple types allowed in make_dyn_function", ""))
  in
  fun make_dyn_type l = make (DYN (make_dyn_function l), make_attrib())
  end

  (* find equiv representative of annotated type *)
  fun ecr t =
      let val eqptr = get #equivptr t
      in case !eqptr of
           None => t
         | Some t' => let val t'' = ecr t'
                      in (eqptr := Some t''; t'')
                      end
      end

  fun zip f ([], []) = ()
    | zip f (a::r, a'::r') = (f (a, a'); zip f (r, r'))
    | zip f (_, _) = raise IllegalInput ("Lists of unequal lengths in zip", "")

  infix ::= 

  (* asymmetric union: contents is the contents of the first element *)
  fun union1 (t1, t2) =
      let val c1 = !!t1
      in (union (t1, t2);
          t1 ::= c1)
      end

  (* asymmetric union: contents is the contents of the second element *)
  fun union2 (t1, t2) =
      let val c2 = !!t2
      in (union (t1, t2);
          t2 ::= c2)
      end

  fun equiv (t1, t2) =
      let val t1' = ecr t1
          val t2' = ecr t2
      in if equal(t1', t2')
            then ()
         else (case (utype t1', utype t2') of
                (DYN f, DYN f') => (union (t1', t2'); 
                                    zip alias (map f type_tags, map f' type_tags))
              | (DYN f, SIMPLE (tt, _)) => alias (f tt, t2)
              | (DYN _, TVAR _) => get #equivptr t2' := Some t1'
              | (SIMPLE (tt, _), DYN f) => alias (t1, f tt)
              | (SIMPLE (tt, _), SIMPLE (tt', _)) => 
                   if tt = tt' then
                      alias (t1, t2)
                   else let val rd = make_dyn_type [t1', t2']
                        in (get #equivptr t1' := Some rd;
                            get #equivptr t2' := Some rd)
                        end
              | (SIMPLE _, TVAR _) => get #equivptr t2' := Some t1'
              | (TVAR _, DYN _) => get #equivptr t1' := Some t2'
              | (TVAR _, SIMPLE _) => get #equivptr t1' := Some t2'
              | (TVAR _, TVAR _) => get #equivptr t1' := Some t2')
       end
  and alias (t1, t2) =
      if equal (t1, t2) 
         then ()
      else ((case (utype t1, utype t2) of
             (SIMPLE (tt, tlist), SIMPLE (tt', tlist')) =>
                if tt = tt' then
                   (union (t1, t2);
                    zip alias (tlist, tlist'))
                else raise IllegalInput ("Equiv error: different tags encountered", "")
           | (TVAR _, SIMPLE _) => union2 (t1, t2)
           | (SIMPLE _, TVAR _) => union1 (t1, t2)
           | (TVAR _, TVAR _) => union (t1, t2)
           | (_,_) => raise IllegalInput ("Illegal type aliasing attempted in alias", "") 
           );
           equiv (t1, t2))

  fun unify (t1, t2) =
      if equal (t1, t2) 
         then ()
      else (case (utype t1, utype t2) of
              (DYN f, DYN f') => (union (t1, t2); 
                                  zip unify (map f type_tags, map f' type_tags))
            | (SIMPLE (tt, tlist), SIMPLE (tt', tlist')) =>
                 if tt = tt' then
                    (union (t1, t2); zip unify (tlist, tlist'))
                 else raise IllegalInput ("Functor clash in unify", "")
            | (TVAR _, TVAR _) => union (t1, t2)
            | (TVAR _, _) => union2 (t1, t2)
            | (_, TVAR _) => union1 (t1, t2)
            | (_, _) => raise IllegalInput ("Dyn and simple types cannot be unified", ""))

  fun pred_succ (l,h) =
      if equal (l, h) 
         then ()
      else let val pred_h = get #preds h
               val succ_l = get #succs l
           in (pred_h := l :: (!pred_h);
             succ_l := h :: (!succ_l))
           end

  (* propgate pos/neg attributes through SVFG *)
  fun propagate ([], []) = ()
    | propagate (tp::rp, ln) =
        let val tatts = attributes tp
            val posptr = #pos tatts
        in if !posptr then 
              propagate (rp, ln)
           else (posptr := true ; 
                (case utype tp of
                   TVAR _ =>
                     propagate (!(#preds tatts) @ rp, ln)
                 | SIMPLE (FUNC, [td,tr]) =>
                     propagate (tr::rp, td::ln)
                 | SIMPLE (_, tlist) =>
                     propagate (tlist @  rp, ln)
                 | DYN _ => raise IllegalInput 
                   ("Not a variable or simple type in propagate", "pos")))
        end
    | propagate ([], tn::rn) = 
       let val tatts = attributes tn
            val negptr = #neg tatts
        in if !negptr then 
              propagate ([], rn)
           else (negptr := true ; 
                (case utype tn of
                   TVAR _ => 
                     propagate ([], !(#succs tatts) @ rn)
                  | SIMPLE (FUNC, [td,tr]) =>
                     propagate ([td], tr::rn)
                 | SIMPLE (_, tlist) =>
                     propagate ([], tlist @ rn)
                 | DYN _ => raise IllegalInput 
                   ("Not a variable or simple type in propagate", "neg")))
        end



  fun foreach f [] = ()
    | foreach f (a::r) = (f a; foreach f r)

  (* interpret a type after equiv'ing and polarity propagation *)
  fun interpret t =
    let val interp = get #interpreted t
    in
    if !interp then () else
    (get #interpreted t := true;
    case utype t of
      SIMPLE (tt, tlist) => foreach interpret tlist 
    | DYN f => foreach interpret (map f type_tags)
    | TVAR _ => 
       let val tequiv = ecr t
       in
       (case utype tequiv of
         TVAR _ => unify (t, tequiv)
       | SIMPLE _ => let val atts = attributes t
                     in if !(#pos atts) andalso !(#neg atts)
                           then fold (fn (tpr,()) => 
                                       case (#1 (!!tpr)) of
                                         TVAR _ => 
                                           let val attstpr = attributes tpr
                                           in if !(#pos attstpr) andalso
                                                 !(#neg attstpr) 
                                              then unify (t, tpr)
                                              else ()
                                           end
                                        | SIMPLE _ => ()
                                        | DYN _ => raise IllegalInput ("Strange stuff", ""))
                                      (!(#preds atts))
                                      ()
                        else unify (t, tequiv)
                     end
       | DYN f => unify (t, tequiv))
       end)
    end

end
end
 
