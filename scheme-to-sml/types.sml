  (* TYPE TAGS *)
  
  datatype type_tag = FUNC | BOOL | NIL | PAIR | CHAR | SYMBOL |
        STRING | NUMBER | VECTOR | UNSPEC 

  val type_tags = [FUNC, BOOL, NIL, PAIR, CHAR, SYMBOL, 
        STRING, NUMBER, VECTOR, UNSPEC]
  
  datatype shade = WHITE | GREY | BLACK

  (* DYNAMIC TYPES *)
  
  datatype utype =
    SIMPLE of type_tag * atype list   
  | DYN of type_tag -> atype
  | TVAR of int
  
  and attributes = 
    ATT of { equivptr: atype Option ref, 
             generic: bool ref,
             preds: atype list ref, 
             succs: atype list ref,
             pos: bool ref, 
             neg: bool ref, 
             interpreted: bool ref,
             instance: atype Option ref, 
             instantiated: bool ref,
             color: shade ref }

  withtype atype = (utype * attributes) UF
  
  fun make_attrib () = 
      ATT { equivptr = ref None, 
            generic = ref false,
            preds = ref nil, 
            succs = ref nil,
            pos = ref false, 
            neg = ref false, 
            interpreted = ref false, 
            instance = ref None,
            instantiated = ref false,
            color = ref WHITE }

  (* selection functions *)
  fun utype (aty: atype) = #1 (!!aty)
  fun attributes aty = (case !!aty of (_, ATT atts) => atts)
  fun get f aty = f (attributes aty)

  (* make new type variables *)
  local 
     val counter = ref 0 
  in
  fun new_typevar () = 
      make (TVAR (!counter), make_attrib()) before counter := !counter + 1
  end

  (* make a new simple type *)  
  fun make_type stype = make (SIMPLE stype, make_attrib())

  fun bool() = make_type (BOOL, [])
  fun char() = make_type (CHAR, [])
  fun string() = make_type (STRING, [])
  fun symbol() = make_type (SYMBOL, [])
  fun number() = make_type (NUMBER, [])
  fun vector t = make_type (VECTOR, [t])
  fun pair(t1,t2) = make_type (PAIR, [t1,t2])
  fun unit() = make_type (NIL, [])
  fun unspec() = make_type (UNSPEC, [])
  fun func(t1,t2) = make_type (FUNC, [t1,t2])

  fun error s = raise IllegalInput s

  (* make a new dynamic type *)
  fun make_dyn_type l = 
      let fun make_dyn_function [] = (fn _ => new_typevar())
            | make_dyn_function (a::r) =
                    (case utype a of
                           SIMPLE (tt, _) => 
                                 (fn tt' => if tt = tt' then a else make_dyn_function r tt')
                         | _ => error ("make_dyn_function", 
                                           "Only simple types allowed in make_dyn_function"))
      in make (DYN (make_dyn_function l), make_attrib()) 
      end

  fun summands f = map f type_tags
  
  (* find equiv representative of annotated type *)
  fun ecr t =
      let val eqptr = get #equivptr t
      in case !eqptr of
           None => t
         | Some t' => let val t'' = ecr t'
                      in (eqptr := Some t''; t'')
                      end
      end

  infix ::= 

  fun equiv (t1, t2): unit =
      let val t1' = ecr t1
          val t2' = ecr t2
      in if equal(t1', t2')
            then ()
         else case (utype t1', utype t2') of
                (DYN f, DYN f') => (union (t1', t2'); 
                                    apply aliassimple (zip (summands f, summands f')))
              | (DYN f, SIMPLE (tt, _)) => 
                                        (get #equivptr t2' := Some t1';
                                         aliassimple (f tt, t2))
              | (DYN _, TVAR _) => get #equivptr t2' := Some t1'
              | (SIMPLE (tt, _), DYN f) => 
                                        (get #equivptr t1' := Some t2';
                                         aliassimple (t1, f tt))
              | (SIMPLE (tt, _), SIMPLE (tt', _)) => 
                   if tt = tt' then
                      aliassimple (t1, t2)
                   else let val rd = make_dyn_type [t1', t2']
                        in (get #equivptr t1' := Some rd;
                            get #equivptr t2' := Some rd)
                        end
              | (SIMPLE _, TVAR _) => get #equivptr t2' := Some t1'
              | (TVAR _, DYN _) => get #equivptr t1' := Some t2'
              | (TVAR _, SIMPLE _) => get #equivptr t1' := Some t2'
              | (TVAR _, TVAR _) => get #equivptr t1' := Some t2'
      end
  and aliassimple (t1, t2): unit =
      if equal (t1, t2) 
         then ()
      else case (utype t1, utype t2) of
             (SIMPLE (_, tlist), SIMPLE (_, tlist')) =>
                                (union (t1, t2);
                 apply aliasvar (zip (tlist, tlist')))
           | (TVAR _, SIMPLE _) => link (t1, t2)
           | (SIMPLE _, TVAR _) => link (t2, t1)
           | (TVAR _, TVAR _) => union (t1, t2)
           | (_,_) => error ("aliassimple", "Illegal type aliasing attempted")
  and aliasvar (t1, t2): unit =
          if equal (t1, t2)
                 then ()
          else case (utype t1, utype t2) of
                     (TVAR _, TVAR _) => (union (t1, t2);
                                                          equiv (t1, t2))
                   | (_, _) => error ("aliasvar", "Arguments must be type variables!")
                   
  fun unify (t1, t2) =
      if equal (t1, t2) 
         then ()
      else case (utype t1, utype t2) of
              (DYN f, DYN f') => (union (t1, t2); 
                                  apply unify (zip (summands f, summands f')))
            | (SIMPLE (tt, tlist), SIMPLE (tt', tlist')) =>
                 if tt = tt' then
                    (union (t1, t2); apply unify (zip (tlist, tlist')))
                 else error ("Type constructor clash in unify", "")
            | (TVAR _, TVAR _) => union (t1, t2)
            | (TVAR _, _) => link (t1, t2)
            | (_, TVAR _) => link (t2, t1)
            | (_, _) => error ("Dyn and simple types cannot be unified", "")

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
                 | _ => error 
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
                 |  _ => error 
                   ("Not a variable or simple type in propagate", "neg")))
        end


 (* interpret a type after equiv'ing and polarity propagation *)
  fun interpret t =
    let val interp = get #interpreted t
    in
    if !interp 
       then () 
    else
    (get #interpreted t := true;
    case utype t of
      SIMPLE (tt, tlist) => apply interpret tlist 
    | DYN f => apply interpret (summands f)
    | TVAR _ => 
       let val tequiv = ecr t
       in
       (case utype tequiv of
         TVAR _ => link (t, tequiv)
       | SIMPLE _ => let val atts = attributes t
                     in if !(#pos atts) andalso !(#neg atts)
                           then apply (fn tpr => 
                                       case utype tpr of
                                         TVAR _ => 
                                           let val attstpr = attributes tpr
                                           in if !(#pos attstpr) andalso
                                                 !(#neg attstpr) 
                                              then union (t, tpr)
                                              else ()
                                           end
                                        | SIMPLE _ => ()
                                        | _ => error ("interpret", 
                                               "Must not be a DYN type"))
                                      (!(#preds atts))
                            else link (t, tequiv)
                     end
       | DYN f => link (t, tequiv))
       end)
    end

  end
end
