fun C sd = QUOTE(SIMPLEDAT(sd));

val five = C (NUMBDAT 5);
    
val lrexp = LETREC ([("x", five)], ([], ([], VARIABLE "x")));
    
val lamexp = LAMBDA ((["x"], NONE), 
		     ([FUNDEF("f", (["x"], NONE), ([], ([], VARIABLE
						   "x")))],
		      ([], VARIABLE "x")));
    
val env1 = add (("g", eval_exp empty_env lamexp), empty_env);
    
val callexp = CALL (VARIABLE "g", [five]);
    

					    
