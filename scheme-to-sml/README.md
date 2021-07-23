Build the `dts` executable by running `make`. You'll need `mlton`.

```ShellSession
$ echo '(define x 5)' | ./dts
number
```

Look! It inferred that 5 is a number!

```ShellSession
$ echo '(define x (lambda (n) 5))' | ./dts
forall 'a44 ~> lst('a43). func(pair('a42, 'a44), number)
mgree@rocinante:~/dts/scheme-to-sml (mlton) $ 
```

It inferred that `n` could have any type (I think?) and that the
result is a number.
