(* Build system *)

val print_depth = ref 5;

use "LIBS.SML";
use "OBJECTS1.SML";
use "OBJECTS2.SML";
use "PHASES.SML";
use "CODEGEN.SML";
use "TYPEDEF.SML";

val _ = print_depth := 40;
use "PROCESS.SML";
