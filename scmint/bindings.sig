(*$ SCHEMEBINDINGS *)

signature BINDINGS =
sig

(* BINDINGS

Created by: Fritz Henglein, DIKU, University of Copenhagen, henglein@diku.dk
Date: 04 Sep 1993

Maintenance: Author

DESCRIPTION

Bindings for all Scheme standard procedures

*)

type variable
type dynamic

val initbindings: (variable * dynamic) list

end

