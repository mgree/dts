(*
$File: Common/PRETTYPRINT.sml $
$Date: 1992/01/29 15:01:51 $
$Revision: 1.6 $
$Locker: birkedal $
*)
(*$PRETTYPRINT*)
(*
                                                     7 Sept. 89 Mads Tofte

This is a pretty-printer in ML which will print everything which you
can represent as a tree of strings.  The interface is given in
the signature PRETTYPRINT below. To use it, you have to build a
`StringTree' which you can then turn into an abstract `minipage' by
calling `format' and then flatten into a single formatted string by
calling `flatten'.

    A StringTree is either a LEAF, containing a string which will not
be decomposed or split over lines; or it is a NODE with a `start' string,
a `finish' string, a list of `children' and a `childsep', which is the
separator that will be used between children if there are more than two.

    A StringTree will be printed on one line, if possible. Otherwise,
it will be printed by putting the children vertically between the
`start' string and the `finish' string.  In the latter case every
child is indented by `indent' blanks.  `start' and `finish' can be
empty strings.  Furthermore, when n children are vertically adjacent
and the separator is LEFT, the separator will appear left-adjusted in
front of the children 2 -- n and if the separator is RIGHT it will
appear appended to the end of child 1 -- n-1.

    In a call `format(hsize: int, t: StringTree)', hsize is the width
in characters of the minipage to be produced. hsize must be >= 3,
so that there is at least space for printing "..." . The meaning of
"..." in the output is : "something goes here, but I don't have the
space to show you what it is".

    Changes (NICK): you are responsible for putting white space in the
start, finish, and childsep strings to ensure that atoms don't get
stuck together. This is neater than giving PrettyPrint a notion of
what things are delimiters and what aren't. So, for (e.g.) a local
declaration, start would be "local ", finish would be " end", and
childsep would (probably, depending on taste) be (LEFT " in ").
LEAF trees don't need surrounding spaces. PrettyPrint removes leading
spaces from any childsep or finish string which ends up at the start
of a line (such as " in " or " end" above), to preserve left
justification.
*)

signature PRETTYPRINT =
  sig
    datatype StringTree = LEAF of string
                        | NODE of {start : string, finish: string, indent: int,
                                   children: StringTree list,
                                   childsep: childsep}
    and childsep = NONE | LEFT of string | RIGHT of string

    type minipage

    val layoutAtom: ('a -> string) -> ('a -> StringTree)
			(* Given a simple printing routine, return a function
			   to build a leaf. *)

    val layoutSet: ('a -> StringTree) -> 'a EqSet.Set -> StringTree

    val flatten1: StringTree -> string
    val oneLiner: ('a -> StringTree) -> ('a -> string)
			(* format a StringTree to a single string of
			   indefinite length. *)

    val format: int * StringTree -> minipage
            (*  ^page width         ^pretty-printed tree *)
            (*  must be >= 3                             *)

    val flatten: minipage -> string
        (* flattens a minipage to a single string putting a newline
           between the lines. *)

    type Report
    val reportStringTree: StringTree -> Report
  end;
