You can run this program by running `sml BUILD.SML`.

```ShellSession
$ echo '(define x 5)' | sml BUILD.SML
...
val poly_type = ([],SIM (NUMBER,[])) :
  (Type.PPtype * Type.PPtype) list * Type.PPtype
val it = () : unit
- - 
```

Look! It inferred that 5 is a number!

```ShellSession
$ echo '(define (x n) 5)' |  sml BUILD.SML
...
val poly_type =
  ([(TYVAR 44,SIM (LST,[TYVAR 43]))],
   SIM (FUNC,[SIM (PAIR,[TYVAR 42,TYVAR 44]),SIM (NUMBER,[])])) :
  (Type.PPtype * Type.PPtype) list * Type.PPtype
val it = () : unit
- - 
```

It inferred that `n` could have any type (I think?) and that the
result is a number.
