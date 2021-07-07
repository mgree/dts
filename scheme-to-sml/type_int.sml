(* 10th stage: interpret types according to information collected *)

  local open General UnionFind Type Constraint

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
  in
  val type_int = foreach (fn (t1,t2) => (interpret t1; interpret t2))
  end
