structure ProcessKernel =
  struct
  local open KernelExp KernelTypes KernelConstraints
  in
 
  fun process ip =
      let val e = read_exp (fn () => (new_typevar(), 
                        new_typevar()), new_typevar) ip  
          val (Cset, VarStruc, hi_type) = Cexp e
          val FVarOccs = free_var_occs VarStruc
          val UnuFormals = unused_formals VarStruc 
          fun get_atype (VAR (_, b)) = b
      in (foreach equiv Cset;
          foreach pred_succ Cset;
          propagate (hi_type :: map get_atype UnuFormals, 
                     map get_atype FVarOccs);
          foreach (fn (l,h) => (interpret l; interpret h)) Cset;  
          e)
      end

  end
end

