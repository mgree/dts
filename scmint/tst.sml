

    val AFR = 
              let val id = (fn x => x) in
               {ann_fcn = fn x => ref (INFO "INFO"),
               ann_lambda = id,
               ann_literal = id,
               ann_variable = id,
               ann_call = id,
               ann_if = id,
               ann_let = id,
               ann_vardef = id,
               ann_fundef = id,
               ann_begindef = id,
               ann_varpar = id,
               ann_pairpar = id,
               ann_command = id,
               ann_definition = id} 
  end

