(*$ SchemeCoercion *)

(* A selection of coercions for embedding procedures into type dynamic *)

structure Coercion =
  struct
  local open Dynamic in

  exception WrongNumberArguments of int

  fun ID f = f
  fun FUNC0 h f = PROCEDURE_TAG (fn nil => h f () |
       			             _ => raise WrongNumberArguments 0)
  fun FUNC0L (g, h) f = PROCEDURE_TAG (fn l => h (f (map g l)))
  fun FUNC1 (g, h) f = PROCEDURE_TAG
        (fn [x] => h (f (g x)) |
            _  => raise WrongNumberArguments 1)
  fun FUNC1L (g, h, i) f = PROCEDURE_TAG
        (fn (x::l) => i (f (g x, map h l)) |
            _ => raise WrongNumberArguments 1)
  fun FUNC2UT (g, h, i) f = (fn [x,y] => i (f (g x, h y)) |
           _ => raise WrongNumberArguments 2)
  fun FUNC2 (g, h, i) = PROCEDURE_TAG o FUNC2UT (g, h, i)
  fun FUNC1O (g, h, i) d f  = PROCEDURE_TAG
        (fn [x] => i (f (g x, d)) |
	   [x,y] => i (f (g x, h y)) |
	       _ => raise WrongNumberArguments 2)
  fun FUNC3 (g, h, i, j) f = PROCEDURE_TAG
	(fn [x,y,z] => j (f (g x, h y, i z)) |
		 _ => raise WrongNumberArguments 3)
  fun FUNC1UT f = (fn [x] => f x |
                      _ => raise WrongNumberArguments 1)
  end
  end

