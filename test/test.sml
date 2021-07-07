val def1 = DEFINITION (VARDEF ("car", QUOTE (NUMBDAT "59")))

val exp1 = COMMAND (CALL (VARIABLE "car", [CALL (VARIABLE "cons", 
					[QUOTE (NUMBDAT "12"), 
					 QUOTE (BOOLDAT "#f")])]))

