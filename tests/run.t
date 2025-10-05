Describe the Alpaca programming language syntax and semantics.

  $ run() { cat > temp; $TEST_DIR/../main.exe run temp "$@"; }

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
  >   let This x = box x,
  >   let x = This x,
  >   match x
  >   | This x: print (unbox (box x))
  > .
  hello, world

Data is matched based on both tag name and arity; that is, if a values tag
matches the pattern's tag, but it has the wrong number of sub-values, then it
won't match.

  $ run <<\.
  > main = fun T: 
  >   match X "a" 2
  >   | X _: print "first pattern\n"
  >   | X _ _: print "second pattern\n"
  > .
  second pattern

FUNCTIONS

Functions in Alpaca accept any number of arguments, and produce one output. All
functions are "anonymous", but can easily be bound to a name using a
let-expression or a top-level definition.

  $ run <<\.
  > f = fun x y z: Triple x y z,
  > g =
  >   let h = fun (Triple x y z): z,
  >   h,
  > main = fun T: print (g (f 1 2 "hello, world\n"))
  > .
  hello, world

Unlike other functional languages, functions are not curried by default.
Passing the wrong number of arguments results in an error.

  $ run <<\.
  > f = fun x y: x,
  > main = fun T: f 1
  > .
  ABORT: Not enough args.
  [1]

  $ run <<\.
  > f = fun x y: x,
  > main = fun T: f 1 2 3
  > .
  ABORT: Too many arguments.
  [1]

Functions can be passed around as values, but cannot be deconstructed via
pattern matching.

  $ run <<\.
  > first = fun (T x y): x,
  > second = fun (T x y): y,
  > print_apply = fun f x: print (f x),
  > main = fun T:
  >   let x = T "first\n" "second\n",
  >   print_apply first x;
  >   print_apply second x
  > .
  first
  second

  $ run <<\.
  > f = fun x: x,
  > main = fun T:
  >   match f
  >   | (fun x: x): print "matched\n"
  > .
  4:6 ABORT: Attempted to use function expression as pattern.
  |   match f
  |   | (fun x: x): print "matched\n"
  \------^
  Specifically this part: `| (fun x`
  [1]

STRING INTERPOLATION

String literals can contain arbitrary sub-expressions, each of which must
evaluate to either a string or a character. If one evaluates to something else, an error will be raised.

  $ run <<\.
  > f = fun x: x,
  > main = fun T:
  >   print "hello, {"worl"}{'d'}\n";
  >   let world = "world",
  >   print "hello, {world}\n";
  >   print "1 2 {int_to_string 3}\n"
  > .
  hello, world
  hello, world
  1 2 3

  $ run <<\.
  > f = fun x: x,
  > main = fun T: print "{1}\n"
  > .
  2:23 ABORT: Attempted to interpolate a non-string value.
  | f = fun x: x,
  | main = fun T: print "{1}\n"
  \-----------------------^
  Specifically this part: ` "{1}\n"`
  [1]

The opening curly-brace can be escaped if it needs to be included in the string
literal. The closing curly-brace doesn't need to be escaped, since it only has
meaning after the opening curly braces switches things into expression mode.

  $ run <<\.
  > f = fun x: x,
  > main = fun T: print "\{}  }\n"
  > .
  {}  }

CHARACTERS

Character literals are denoted by enclosing a character (possibly escaped) in
single quotes. They can be used both as data, and also patterns for
matching data.

  $ run <<\.
  > main = fun T:
  >   match 'c'
  >   | 'a': print "a\n"
  >   | 'c': print "c\n"
  > .
  c

INTEGERS

Integer literals are denoted by a sequence of digits. They can be used both as
data, and also patterns for matching data.

  $ run <<\.
  > main = fun T:
  >   match 2
  >   | 1: print "1\n"
  >   | 2: print "2\n"
  > .
  2

PRIMITIVE OPERATIONS

Alpaca comes with an array of primitive functions for dealing with all the
primitive kinds of values.

The "print" function writes some output to the standard output stream.

  $ run <<\.
  > main = fun T: print "hello, world\n"
  > .
  hello, world

Command-line arguments are provided in the array "argv". Each item in the array
can accessed by index.

  $ run arg1 arg2 <<\.
  > main = fun T:
  >   print "number of command-line arguments: {int_to_string (array_length argv)}\n";
  >   print "command-line arguments: {array_get argv 0} {array_get argv 1} {array_get argv 2}\n"
  > .
  number of command-line arguments: 3
  command-line arguments: temp arg1 arg2

Integers can be compared and converted to strings.

  $ run <<\.
  > f = fun x:
  >   let relation =
  >     match int_compare x 3
  >     | Less_than: "less than"
  >     | Greater_than: "greater than"
  >     | Equal: "equal to",
  >   print "{int_to_string x} is {relation} 3\n",
  > main = fun T:
  >   f 2;
  >   f 3;
  >   f 4
  > .
  2 is less than 3
  3 is equal to 3
  4 is greater than 3

Strings can be indexed and measured.

  $ run <<\.
  > main = fun T:
  >   let s = "hello",
  >   print "length: {int_to_string (string_length s)}\n";
  >   print "first character: {string_get s 0}\n"
  > .
  length: 5
  first character: h

Files can be read into string values.

  $ cat > file.txt <<\.
  > hello, world
  > .
  $ run <<\.
  > main = fun T:
  >   let contents = read_file "file.txt",
  >   print contents
  > .
  hello, world

The program can abort itself. This is not an exception mechanism, since there
is no way to catch the abort. This is merely a way to fail fast if an
unexpected condition is encountered.

  $ run <<\.
  > main = fun T: abort "quitting"
  > .
  quitting
  [1]
