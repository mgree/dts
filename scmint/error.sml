structure Error: ERROR =
  struct

  exception Unimplemented of string
  and Impossible of string
  and TypeError of string * object
  and InputError of string * object
  and IOError of string * string
  and EndOfFile

  fun type_error x = raise TypeError x
  fun input_error x = raise InputError x
  fun IO_error x = raise IOError x
  fun unimplemented x = raise Unimplemented x
  fun impossible x = raise Impossible x
  end
