structure Term =
struct
  local open UnionFind 
  in

  datatype ('t, 'v, 'a) uterm =
    VAR 
  | APP of ''t * ('a, ''t) term list
  withtype ('a, ''t) term = (('a, ''t) uterm * 'a) UF

  local 
  val varcounter = ref 0
  val ttcounter = ref 0  
  in fun new_var a = 
         (counter := !counter + 1; 
          make (VAR (!counter), a))
  end

  fun new_term (app, a) = 
      make (APP app, a)

  fun case_term vf af (t: ('a, 't) term) =
      let val ct = !!t
      in case #1 ct of
           VAR v => vf v (#2 ct)
         | APP (tt, tlist) => af tt tlist (#2 ct)
      end

  exception TermError of string

  fun zip f ([], []) = ()
    | zip f (a::r, a'::r') = (f (a, a'); zip f (r, r'))
    | zip f (_, _) = raise TermError 
                ("Lists of unequal lengths in zip")

  fun unify (t1, t2) =
      case (#1 (!!t1), #1 (!!t2)) of
        (VAR _, VAR _) => union #2 (t1, t2)
      | (VAR _, APP _) => union #2 (t1, t2)
      | (APP _, VAR _) => union #1 (t1, t2)
      | (APP (tt, tlist), APP (tt', tlist')) =>
             if tt = tt' then
                 (union #2 (t1, t2);
                  zip unify (tlist, tlist'))
             else raise TermError ("Function symbol clash")
               
  end
end
