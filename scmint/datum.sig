signature SCHEMEDATUM =
  sig

  datatype ('a, 'd) datum_hom =
      DHOM of { booldat: 'a -> bool -> 'd,
                chardat: 'a -> string -> 'd,
                stridat: 'a -> string -> 'd,
                symbdat: 'a -> string -> 'd,
                numbdat: 'a -> string -> 'd,
                vectdat: 'a -> 'd list -> 'd,
                pairdat: 'a -> 'd * 'd -> 'd,
                nildat: 'a -> 'd 
              }

  datatype 'a datum =
      BOOLDAT of bool |
      CHARDAT of string |
      STRIDAT of string |
      SYMBDAT of string |
      NUMBDAT of string |
      VECTDAT of 'a anndatum list |
      PAIRDAT of 'a anndatum * 'a anndatum |   
      NILDAT

  and 'a anndatum =
      DATUM of 'a datum * 'a

  val apply_dhom: ('a, 'd) datum_hom -> 'a anndatum -> 'd

  exception ReadError of string * string
  exception EOF

  val read_datum: (unit -> 'a) -> instream -> 'a anndatum

  end
