(* build system *)

val pd = SML_NJ.Print.printDepth;
val _ = pd := 5;

use "libs.sml";
use "objects1.sml";
use "objects2.sml";
use "phases.sml";
use "codegen.sml";
use "typedef.sml";

val _ = pd := 40;
fun proc() = use "process.sml";
output(std_out, 
       "Execute \"proc()\"  and input Scheme definition\n");
