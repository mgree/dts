(* Identifiers - variables or constructors *)

(*
$File: Common/Ident.sml $
$Date: 1992/08/19 14:46:01 $
$Revision: 1.22 $
$Locker: birkedal $
*)

(*$Ident: STRID TIMESTAMP CRASH IDENT*)
functor Ident(structure StrId: STRID
	      structure Timestamp: TIMESTAMP
	      structure Crash: CRASH
	     ): IDENT =
  struct
    type strid = StrId.strid

    datatype id = ID of string

    fun pr_id(ID str) = str

    datatype longid = LONGID of id list * id

    fun pr_longid(LONGID(ids, id)) =
      let
	val strings = (map (fn s => pr_id s ^ ".") ids)
      in
	List.foldR (General.curry op ^) (pr_id id) strings
      end

    fun unqualified longid =
      case longid
	of LONGID(nil, _) => true
	 | _ => false

    fun decompose(LONGID(ids, id)) =
      (map (fn ID x => StrId.mk_StrId x) ids, id)

    fun decompose0 longid =
      case decompose longid
	of (nil, id) => id
	 | _ => Crash.impossible "Ident.decompose0"

    fun (ID str1) < (ID str2) = AsciiOrdString.lt str1 str2

   (* Identifiers needed for derived forms: *)

    val id_NIL = ID "nil"
    and id_CONS = ID "::"
    and id_TRUE = ID "true"
    and id_FALSE = ID "false"
    and id_REF = ID "ref"
    and id_PRIM = ID "prim"
    and id_IT = ID "it"

    (* Identifiers for predefined overloaded variables *)
    val id_ABS = ID "abs"
    val id_NEG = ID "~"
    val id_PLUS = ID "+"
    val id_MINUS = ID "-"
    val id_MUL = ID "*"
    val id_LESS = ID "<"
    val id_GREATER = ID ">"
    val id_LESSEQ = ID "<="
    val id_GREATEREQ = ID ">="

    val bogus = LONGID (nil, ID "<bogus>")

    val mk_Id = ID

    fun mk_LongId strs =
      case (rev strs)
	of nil => Crash.impossible "Ident.mk_LongId"
	 | x :: xs => LONGID(map ID (rev xs), ID x)

    local
      fun unique() = "<unique_Id." ^ Timestamp.print(Timestamp.new()) ^ ">"
    in
      val inventId = ID o unique
      fun inventLongId() = LONGID(nil, inventId())
    end

    fun idToLongId id = LONGID(nil, id)
  end;
