(* structure Error: ERROR =
  struct *)

  exception Unimplemented of string
  and Impossible of string
  and ParseError of string * datum
  and TypeError of string * object
  and InputError of string * object
  and IOError of string * string
  and EndOfFile
(*
  end
*)