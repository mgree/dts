(* Build system *)

val pd = SML_NJ.Print.printDepth;
val _ = pd := 5;

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



