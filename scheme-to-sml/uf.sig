(*$UNIONFIND: *)

signature UNIONFIND =
sig

(* UNIONFIND PACKAGE

Created by: Fritz Henglein, DIKU, University of Copenhagen
Date:       Dec. 1994

Maintenance: Author


DESCRIPTION

Union/Find data type with ref-like interface.  A Union/Find structure 
consists of a type constructor 'a UF with operations for
making an element of 'a UF (make), getting the contents of
an element (!!), checking for equality of two elements (equal), and
for joining two elements (union).  UF is analogous to ref as
expressed in the following table:

-------------------------------------------------------
type                  'a ref                'a UF
-------------------------------------------------------
introduction          ref                   make
elimination           !                     !!
equality              =                     equal
updating              :=                    ::=
unioning                                                                        union
--------------------------------------------------------

The main difference between 'a ref and 'a UF is in the union 
operation.  Without union 'a ref and 'a UF can be 
used interchangebly.  An assignment to a reference changes only 
the contents of the reference, but not the reference itself.
In particular, any two pointers that were different (in the 
sense of the equality predicate = returning false) before an
assignment will still be so.  Their contents may or may not be 
equal after the assignment, though.  In contrast, applying the 
union operation to two UF elements makes the two elements 
themselves equal (in the sense of the predicate equal returning
true).  As a consequence their contents will also be identical: 
it will be the contents of one of the two unioned elements.


EXAMPLES


RCS LOG

*)


(* TYPES *)

  type 'a UF
  (* type of UF-elements with contents of type 'a *)  


(* CREATORS *)
      
  val make: '_a -> '_a UF
  (* make x makes a new element with contents x *)


(* OBSERVERS *)

  val equal: 'a UF * 'a UF -> bool
  (* equal (e, e') returns true if and only if e and e' are either made by
     the same call to make or if they have been unioned (see below) *)

(* SELECTORS *)

  val !! : 'a UF -> 'a
  (* !!e returns the contents of e. 
     Note: if 'a is an equality type then !!(make x) = x, and 
     equal(make (!!x), x) = false *)


(* MANIPULATORS *)

  val ::= : 'a UF * 'a -> unit
  (* := (e, x) updates the contents of e to be x *)

  val union: 'a UF * 'a UF -> unit
  (* union (e, e') makes e and e' equal; the contents of the unioned
     element is the contents of one of e and e' before the union operation.
     After union(e, e') elements e and e' will be congruent in the
     sense that they are interchangeable in any context *)


end;
