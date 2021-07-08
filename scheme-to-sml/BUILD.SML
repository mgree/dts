(* Build system *)

use "LIBS.SML";
use "OBJECTS1.SML";
use "OBJECTS2.SML";
use "PHASES.SML";
use "CODEGEN.SML";
use "TYPEDEF.SML";

val _ = pd := 40;
fun proc() = use "PROCESS.SML";
output(std_out, 
       "Execute \"proc()\"  and input Scheme definition\n");



