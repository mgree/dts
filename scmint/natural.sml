(*$ NATURAL *)

signature NATURAL =
sig

(* NATURAL

Created by: 
 Fritz Henglein, Jakob Rehof,
 DIKU, University of Copenhagen, 
 henglein@diku.dk, rehof@diku.dk
Date: Jan 5, 1994

Maintenance: Authors

DESCRIPTION

Standard procedures for natural numbers

*)

(* TYPES *)

type nat
type T
sharing type T = nat

(* INPUT/OUTPUT *)

(*
val parse: string -> (T * string, T Option * string) Result
val read: instream -> (T, T Option) Result
val string: T -> string
val print: outstream -> T -> unit
*)

(* CONSTRUCTORS *)

val + : nat * nat -> nat
val * : nat * nat -> nat
val - : nat * nat -> nat
val div: nat * nat -> nat
val mod: nat * nat -> nat
val max: nat * nat -> nat
val min: nat * nat -> nat
val even: nat -> bool
val odd: nat -> bool
val < : nat * nat -> bool
val <= : nat * nat -> bool
val > : nat * nat -> bool
val >= : nat * nat -> bool
val ** : nat * nat -> nat

end


(*$ Natural: NATURAL *)

structure Natural: NATURAL =
  struct

  type nat = int
  nonfix + * - div mod < <= >= > **

  val + = ( + : int * int -> int)
  val * = ( * : int * int -> int)
  val - = fn (x,y) => if < (x,y) then 0 else - (x, y)
  val div = div
  val mod = mod
  val max = max
  val min = min
  fun even n = mod (n, 2) = 0
  fun odd n = mod (n, 2) = 1
  val < = (< : int * int -> bool)
  val <= = (<= : int * int -> bool)
  val > = (> : int * int -> bool)
  val >= = (>= : int * int -> bool)
  fun ** (n, m) = 
      if m = 0 then 1
      else if n = 0 then 0
	   else let val root = ** (n, div (m, 2))
		in if even m 
		       then * (root, root)
		   else * (* (root, root), n)
		end

  end
 
