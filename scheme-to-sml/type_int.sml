structure SchemeTypeInterpret =
  struct

  local open SchemeTypes KernelConstraints KernelVarUsage
  in

  (* 9th stage: propagate positivity all throughout the constraints *)

  (* done by using function propagate on the type variables contained
     in the free_pos_vars plus the result type as positive type variables,
     and the types in the free variables as negative type variables *)

  (* 10th stage: interpret types according to information collected *)

  val typeint = foreach (fn (t1,t2) => (interpret t1; interpret t2))

  end

  end
