use "types.sml";
use "scheme.sig"; 
use "error.sml";
use "conv.sml";
use "object.sml";
use "pair.sml";
use "list.sml";
use "symbol.sml";
use "number.sml";
use "char.sml";
use "string.sml";
use "vector.sml";
use "control.sml";
use "io.sml";
use "datum.sml";
use "command.sml";
use "env.sml";
use "eval.sml";
open Eval;
val run = fn () => run (std_in, std_out);
exportML "DTS";
