(* The pretty-printer *)

(*
$File: Common/PrettyPrint.sml $
$Date: 1995/04/17 07:55:32 $
$Revision: 1.2 $
$Locker:  $
*)

(*$PrettyPrint: CRASH REPORT PRETTYPRINT *)

functor PrettyPrint(structure Report: REPORT
		    structure Crash: CRASH
		   ): PRETTYPRINT =
  struct
    val WIDTH = 75
    val DEBUG = false

   (* put all tokens on one line *)
    val collapse: string list -> string =
      List.foldR (General.curry op ^) ""

    val flattenList: 'a list list -> 'a list =
      List.foldR (General.curry op @) nil
	  
    datatype childsep = NONE | LEFT of string | RIGHT of string

    datatype StringTree =
        NODE of {start : string, finish: string, indent: int,
	         children: StringTree list, childsep: childsep
	        }
      | LEAF of string
	
    fun layoutAtom (f: 'a -> string) (x: 'a) = LEAF(f x)
      
    fun layoutSet f set =
      NODE{start="{", finish="}", indent=1, childsep=RIGHT ", ",
	   children=map f (EqSet.list set)
	  }

    exception FlatString
    fun consIfEnoughRoom(s,(acc: string list, width: int)) = 
      let val n = size s 
      in
         if n<= width then ((s::acc), width-n) else raise FlatString
      end

    local

      fun fold f (LEAF s, acc) = f(s, acc)
	| fold f (NODE{start, finish, indent, children, childsep}, acc) =
	    f(finish, foldChildren f (children, childsep, f(start, acc)))

      and foldChildren f (nil, childsep, acc) = acc
	| foldChildren f ([t], _, acc) = fold f (t, acc)

	| foldChildren f (child :: rest, NONE , acc) = 
            foldChildren f (rest, NONE, fold f (child, acc))

	| foldChildren f (child :: rest, RIGHT s, acc) = 
            foldChildren f (rest, RIGHT s, f(s, fold f (child, acc)))

	| foldChildren f (child::rest, LEFT s, acc) =
            foldChildren f (rest, LEFT s, f(s, fold f (child, acc)))
    in
      val flatten: StringTree -> string list = fn t => rev(fold (op ::) (t, nil))
      val flatten1: StringTree -> string = collapse o flatten
      fun flattenOrRaiseFlatString(t,width) = collapse(rev(#1(fold consIfEnoughRoom (t, (nil,width)))))
    end
    
    fun oneLiner (f: 'a -> StringTree) (x: 'a) = flatten1(f x)
      
    type minipage = string list
      
    fun blanks n =
      if n <= 0 then "" else " " ^ blanks(n-1)

   (* smash() - tries to prefix "line" (which will probably have some leading
      spaces) with "prefix", by replacing line's leading spaces with the
      prefix. If successful, returns a list of the new line :: rest. If
      not, returns prefix :: line :: rest. *)

    fun smash(prefix, line, rest): string list =
      let
	val prefix' = explode prefix
	val line' = explode line

	exception No
	fun try(p :: pRest, " " :: lRest) = p :: try(pRest, lRest)
	  | try(nil, lRest) = lRest
	  | try(pRest, nil) = pRest
	  | try(_, _) = raise No
      in
	implode(try(explode prefix, explode line)) :: rest
	handle No => prefix :: line :: rest
      end

    fun topLeftConcat (s: string) (m: minipage): minipage =
      case m
	of nil => nil
	 | this :: rest => smash(s, this, rest)

    fun botRightConcat (s: string) (m: minipage): minipage =
      case rev m
	of nil => nil
	 | this :: rest => rev((this ^ s) :: rest)

    fun indent (i: int) (m: minipage) =
      let
	val blanks = blanks i
      in
	map (fn line => blanks ^ line) m
      end

   (* strip() - remove leading spaces from a string. The StringTree might
      well have leading spaces in separators and finish tokens, which is all
      well-and-good for single line printing, but not wanted for multi-line
      printing where LEFT separators and finish tokens appear at the
      start of lines. *)

    fun strip s =
      let
	fun strip'(" " :: rest) = strip' rest
	  | strip' s = s
      in
	(implode o strip' o explode) s
      end

    fun print (width: int) (LEAF s): minipage =	(* width >= 3 *)
          if size s <= width then [s] else ["..."]

      | print (width: int)
	      (t as NODE{start, finish, indent, children, childsep}) =
	  let				(* Try to make it go into just one line *)
	    val flatString: string = flattenOrRaiseFlatString(t,width)
	  in
            [flatString]
          end handle FlatString => (* one line does not hold the whole tree *)
	      let
		val startLines = if start = "" then nil else [start]
		val finishLines = if finish = "" then nil else [strip finish]
	      in
		if size start <= width andalso size finish <= width
		then			(* print children indented *)
		  if width - indent >= 3
		  then			(* enough space to attempt printing
					     of children *)
		    let
		      val childrenLines: minipage = 
			pileChildren(width, indent, childsep, children)

		      val startAndChildren: minipage =
			case childrenLines
			  of nil => startLines

			   | (hd :: tl) =>
				(* `smash' sees if start and hd can be
				   collapsed into one line *)
			       smash(start, hd, tl)

			val allLines =	(* add finishing line, if not empty *)
			  startAndChildren @ finishLines
		    in
		      allLines
		    end 
		  else			(* not enough space to attempt
					     printing of the children *)
		    case children
		      of nil => startLines @ finishLines
		       | _ => startLines @ ["..."] @ finishLines
		else			(* start or finish to big: *)
		  ["..."]
	      end


   (* change to pileChildren. Before, the print function would call
      pileChildren and indent the result. This is wrong for things like
      "let ... in ... end", since the "in" would be a LEFT-separator
      attached to the children, and would be indented from the "let"
      and "end". So now, pileChildren is responsible for indenting
      its own argument. Slight inconvenience, since the caller now
      has to smash together lines where the opening bracket will
      fit on the first line (e.g. "let val ...."). *)

    and pileChildren(width, ind, childsep, nil): minipage = nil

      | pileChildren(width, ind, childsep, [child]) =
	  indent ind (print (width-ind) child)

      | pileChildren(width, ind, NONE, children) =
	  indent ind (flattenList(map (print (width-ind)) children))

      | pileChildren(width, ind, LEFT s, first :: rest) =
	  let
	    val s = strip s	(* If we're printing children multi-line, we
				   *always* take off leading spaces from
				   the separator. *)
	    val firstWidth: int = width - ind
	    val restWidth = width - ind - size s
	  in
	    if restWidth < 3 then
	      indent ind ["..."]
	    else
	      let
		val restPages = map ((indent ind) o (print restWidth)) rest
		val restPages' =
		  map (topLeftConcat s) restPages
	      in
		indent ind (print (firstWidth + size s) first)
		@ flattenList restPages'
	      end
	  end

      | pileChildren(width, ind, RIGHT s, children) =
	  let
	    val myWidth = width - ind (* - size s *)
					(* We ignore the right sep's width. *)
	  in
	    if myWidth < 3 then
	      indent ind ["..."]
	    else
	      let
		val (last, firstN) =
		  case rev children
		    of x :: y => (x, rev y)
		     | _ => Crash.impossible "PP.pileChildren"

		val firstNPages = map (print myWidth) firstN
		val firstNPages' = map (botRightConcat s) firstNPages
	      in
		indent ind (flattenList firstNPages'
			    @ print myWidth last
			   )
	      end
	  end


    fun prSep NONE = "NONE"
      | prSep(LEFT s) = "LEFT \"" ^ s ^ "\""
      | prSep(RIGHT s) = "RIGHT \"" ^ s ^ "\""

    fun printStringTree t: minipage =
      case t
	of LEAF s =>
	     ["LEAF \"" ^ s ^ "\""]

	 | NODE{start, finish, indent=i, children, childsep} =>
	     implode ["NODE(\"", start, "\", \"", finish,
		      "\", ", Int.string i, ", ",
		      prSep childsep, ")"
		     ]
	     :: flattenList(map (fn x => indent 3 (printStringTree x))
			    children
			   )

    fun format(width: int, t: StringTree): minipage =
      if width < 3 then
	Crash.impossible "PrettyPrint.format: width too small"
      else
	print width t
	     
    (* result with newlines  *)
    and flatten (m : minipage) : string =
      List.stringSep "" "" "\n" (fn x => x) m

    type Report = Report.Report

    fun reportStringTree tree =
      let
	val page = format(WIDTH, tree)
	val lines = map Report.line page
      in
	Report.flatten lines
      end
  end;
