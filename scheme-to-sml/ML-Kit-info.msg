From mael@diku.dk  Fri Aug 16 13:02:43 1996
Return-Path: mael
Date: Fri, 16 Aug 1996 13:02:40 +0200
From: Martin Elsman <mael@diku.dk>
To: henglein@diku.dk
Subject: The Kit and its intermediate lambda language.

Hi Fritz,

A signature for the language is in

	~mael/kit/src/Compiler/Lambda/LAMBDA_EXP30b.sml

And its implementation in 

	~mael/kit/src/Compiler/Lambda/LambdaExp30c.sml

These modules does not work alone. In particular, the modules

	~mael/kit/src/Compiler/CompileAndRun30m.sml
	~mael/kit/src/Compiler/Evaluation31b.sml
	~mael/kit/src/Common/KitCompiler29h.sml

connect the system. You probably need to construct your own
`building modules'. 

Check out

	~mael/kit/src/ML_CONSULT_HPPA31c

to see what files (module versions) work together. We use the
Edinburgh Library and the Edinburgh Make system to build the system.

Also, check out the ML Kit home page for more information:

	http://www.diku.dk/research-groups/topps/activities/mlkit.html

-- 
-- Martin
+--------------------------------------------------------------------------+
| Martin Elsman * DIKU, University of Copenhagen * Universitetsparken 1    |
| DK-2100 Copenhagen East, DENMARK * +45 35 321 429 * fax: +45 35 321 401  |
| mael@diku.dk * http://www.diku.dk/research-groups/topps/people/mael.html |
+--------------------------------------------------------------------------+


