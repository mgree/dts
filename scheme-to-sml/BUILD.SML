(* Build system *)

val print_depth = ref 5;

use "LIBS.SML";
use "OBJECTS1.SML";
use "OBJECTS2.SML";
use "PHASES.SML";
use "CODEGEN.SML";
use "TYPEDEF.SML";

val _ = print_depth := 40;
fun proc() = use "PROCESS.SML";
open TextIO;
output(stdOut, 
       "Execute \"proc()\"  and input Scheme definition\n");



