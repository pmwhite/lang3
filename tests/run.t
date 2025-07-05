Describe the Alpaca programming language syntax and semantics.

  $ run() { cat > temp; $TEST_DIR/../main.exe run temp; }

MAIN

All good programming languages begin with a hello, world program, just to give
you a flavor. Here is what it looks like for Alpaca.

  $ run <<\.
  > main = fun T: print "hello, world\n"
  > .
  hello, world

When the interpreter starts up it evaluates all the top-level definitions and
then looks for a definition "main". If such a definition exists, it will call
it with the value "T" (uppercase names refer to data constructors). In the
above program, we see a definition of a "main" function that pattern matches on
"T" and then invokes the "print" function on the hello world string.

It's worth pointing out a few ways that you could get this wrong. If there is
no main function, an error is raised after all the definitions are evaluated.

  $ run <<\.
  > x = print "hi\n"
  > .
  hi
  ABORT: Name 'main' is not defined.
  [1]

If the "main" is not a function, a runtime error will be raised when the
interpreter attempts to call it.

  $ run <<\.
  > main = print "hi\n"
  > .
  hi
  ABORT: No patterns matched value:
  T T
  [1]

If "main" is a function that matches on something other than "T", an error will
be raised.

  $ run <<\.
  > main = fun X: print "hi\n"
  > .
  ABORT: No patterns matched value.
  [1]

VALUES AND PATTERN MATCHING

Values in Alpaca can be integers, strings, characters, functions, arrays, or
"data". Data consists of a tag name and zero or more values; it's very similar
to data in Haskell or OCaml.

Values can be pattern matched in any context where variables can be introduced.
This includes let-expressions, match-expressions, and functions. Here is an
example that illustrates all three.

  $ run <<\.
  > unbox = fun (This x): x,
  > box = fun x: This x,
  > main = fun T: 
  >   let x = "hello, world\n",
  >   let (This x) = box x,
  >   let x = This x,
  >   match x
  >   | This x: print (unbox (box x))
  > .
  hello, world

$ main <<\.
> main = fun T:
>   let x = 0,
>   let f = fun T: x,
>   print "{int_to_string (f T)}\n"
> .
0

$ main <<\.
> f = fun x:
>   match int_compare x 10
>   | Less_than: print "less than 10\n"
>   | Greater_than: print "greater than 10\n"
>   | Equal: print "equal to 10\n",
> main = fun T: f 9; f 10; f 11
> .
less than 10
equal to 10
greater than 10
