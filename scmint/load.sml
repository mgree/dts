app use ["general.sml", "datum.sml", "command.sml",
	 "bool.sml", "pair.sml", "list.sml", "symbol.sml", "number.sml", 
	 "char.sml", "string.sml", "vector.sml", "control.sml", "io.sml",
	 "dynamic.sml", "env.sml", "basis.sml", "runtime.sml", "interpr.sml"];

structure I = Interpret (Runtime);
open I;
