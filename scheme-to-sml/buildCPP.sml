use "usefileC.sml";



structure GrammarInfo =
struct
          type GrammarInfo = unit

end

(*
structure Id =
struct
          type id = string
          
          fun pr_id s = s 
end
*)



structure BasicIO = BasicIO();
structure Crash = Crash(structure BasicIO = BasicIO);
structure Report = Report(structure BasicIO = BasicIO);
structure PP = PrettyPrint(structure Report = Report
                           structure Crash = Crash);
structure SCon = SCon();
structure Lab = Lab();
structure Timestamp = Timestamp();
structure StrId = StrId(structure Timestamp = Timestamp
                        structure Crash = Crash);

structure Ident = Ident(structure StrId = StrId
                        structure Timestamp = Timestamp
                        structure Crash = Crash);
structure Con = Con(structure Ident = Ident);
structure TyVar = TyVar(structure Crash = Crash);
structure TyCon = TyCon(structure StrId = StrId
                        structure Crash = Crash);
structure Excon = Excon(structure Ident = Ident);




structure DecGrammar = DecGrammar(structure GrammarInfo = GrammarInfo
                                  structure Lab = Lab
                                  structure SCon = SCon
                                  structure Con = Con
                                  structure TyVar = TyVar
                                  structure TyCon = TyCon
                                  structure Excon = Excon
                                  structure StrId = StrId
                                  structure Ident = Ident
                                  structure Id = Ident);
                                  



structure Operator = Operator(structure Ident = Ident); 

structure PPDecGrammar = PPDecGrammar(structure DecGrammar = DecGrammar
				      structure SCon = SCon
                                      structure Lab = Lab
                                      structure Con = Con
                                      structure Excon = Excon
                                      structure Ident = Ident
                                      structure Id = Ident
                                      structure Operator = Operator 
                                      structure TyVar = TyVar
                                      structure TyCon = TyCon
                                      structure StrId = StrId
				      structure PP = PP
				      structure Crash = Crash);







structure PrettyPrint =
struct
local open DecGrammar in


fun PPsmlExp x = 
        (output(std_out, "OUTPUT FROM PPsmlAst :\n");
         Report.print (PP.reportStringTree (PPDecGrammar.layoutExp x));
         output(std_out, "OUTPUT END\n"))

fun PPsmlDec x = Report.print(PP.reportStringTree (PPDecGrammar.layoutDec x))



end
end





