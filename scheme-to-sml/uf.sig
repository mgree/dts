(*$UNIONFIND: *)

signature UNIONFIND =
sig

(* UNIONFIND PACKAGE

Created by: Fritz Henglein, DIKU, University of Copenhagen
Date:       Dec. 1994

Maintenance: Author


DESCRIPTION

Union/Find data type with ref-like interface.  A Union/Find structure 
consists of a type constructor 'a uref with operations for
making an element of 'a uref (make), getting the contents of
an element (!!), checking for equality of two elements (equal), and
for joining two elements (link, union, union_with).  
uref is analogous to ref as expressed in the following table:

-------------------------------------------------------------------
type                  'a ref                'a uref
-------------------------------------------------------------------
introduction          ref                   uref
elimination           !                     !!
equality              =                     equal
updating              :=                    ::=
unioning                                    link, union, union_with
-------------------------------------------------------------------

The main difference between 'a ref and 'a uref is in the union
operation.  Without unioning operations 'a ref and 'a uref can be used
interchangebly.  An assignment to a reference changes only the
contents of the reference, but not the reference itself.  In
particular, any two pointers that were different (in the sense of the
equality predicate = returning false) before an assignment will still
be so.  Their contents may or may not be equal after the assignment,
though.  In contrast, applying the union operations (link, union,
union_with) to two uref elements makes the two elements themselves
equal (in the sense of the predicate equal returning true).  As a
consequence their contents will also be identical: in the case of link
and union it will be the contents of one of the two unioned elements,
in the case of union_with the contents is determined by a binary
function parameter.


EXAMPLES

RCS LOG

*)


(* TYPES *)

  type 'a uref
  (* type of uref-elements with contents of type 'a *)  


(* CREATORS *)
      
  val uref: '_a -> '_a uref
  (* uref x creates a new element with contents x *)


(* OBSERVERS *)

  val equal: 'a uref * 'a uref -> bool
  (* equal (e, e') returns true if and only if e and e' are either made by
     the same call to uref or if they have been unioned (see below) *)

(* SELECTORS *)

  val !! : 'a uref -> 'a
  (* !!e returns the contents of e. 
     Note: if 'a is an equality type then !!(uref x) = x, and 
     equal(uref (!!x), x) = false *)


(* MANIPULATORS *)

  val ::= : 'a uref * 'a -> unit
  (* := (e, x) updates the contents of e to be x *)

  val union_with: ('a * 'a -> 'a) -> 'a uref * 'a uref -> unit
  (* union_with f (e, e') makes e and e' equal; if v and v' are the 
     contents of e and e', respectively, before unioning them, 
     then the contents of the unioned element is f(v,v') *)

  val union: 'a uref * 'a uref -> unit
  (* union (e, e') makes e and e' equal; the contents of the unioned
     element is the contents of one of e and e' before the union operation.
     After union(e, e') elements e and e' will be congruent in the
     sense that their values are interchangeable in any context.  *)

  val link: 'a uref * 'a uref -> unit
  (* link (e, e') makes e and e' equal; the contents of the linked
     element is the contents of e' before the link operation *)

end;
