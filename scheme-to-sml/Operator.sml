(*
$File: Common/Operator.sml $
$Date: 1995/04/17 07:55:32 $
$Revision: 1.2 $
$Locker:  $
*)

(*$Operator: OPERATOR IDENT
 *)

functor Operator(structure Ident : IDENT) : OPERATOR =
  struct
    structure Ident = Ident
    type id = Ident.id
    datatype assoc = LEFT | RIGHT
    type infixenv = (string * int * assoc) list

    exception opNotFound
      
    val basisenv  = [(":=",  3, RIGHT), ("o",  3, LEFT),
		     ("=",   4, LEFT),  ("<>", 4, LEFT),  ("<",   4, LEFT),
		     (">",   4, LEFT),  ("<=", 4, LEFT),  (">=",  4, LEFT),
		     ("@",   5, LEFT),  ("::", 5, RIGHT),
		     ("+",   6, LEFT),  ("-",  6, LEFT),  ("^",   6, LEFT),
		     ("*",   7, LEFT),  ("/",  7, LEFT),  ("div", 7, LEFT),
		     ("mod", 7, LEFT)]

    local
      fun lookupOp' [] opStr = raise opNotFound
	| lookupOp' ((opStr', opPrec, opAss) :: ops) opStr =
	    if opStr' = opStr then
	      (opPrec, opAss)
	    else
	      lookupOp' ops opStr
      fun registerInfix' assoc prec idstr [] = [(idstr, prec, assoc)]
	| registerInfix' assoc prec idstr ((s, p, a) :: spas) =
	    if s = idstr then
	      (s, prec, assoc) :: spas
	    else
	      (s, p, a) :: registerInfix' assoc prec idstr spas
    in
      fun isInfix [] id = false
	| isInfix ((s,_,_) :: spas) id =
 	    s = Ident.pr_id id orelse isInfix spas id
      fun registerInfixr infixenv prec ids =
	let
	  val prec = case prec of Some prec => prec | None => 0
	in
	  List.foldR (registerInfix' RIGHT prec) infixenv
	  (map Ident.pr_id ids)
	end

      fun registerNonfix infixenv ids =
	List.foldR
	  (fn idstr => fn env =>
	   List.dropAll (fn (s,_,_) => s = idstr) env)
	  infixenv (map Ident.pr_id ids)
	  
      fun registerInfix infixenv prec ids =
	let
	  val prec = case prec of Some prec => prec | None => 0
	in
	  List.foldR (registerInfix' LEFT prec) infixenv
				 (map Ident.pr_id ids)
	end
 
	
      fun lookupOp infixenv id = lookupOp' infixenv (Ident.pr_id id)
    end
  end
 
