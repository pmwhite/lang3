Demonstrate the behavior of the autoformat command of the Alpaca programming
language.

  $ main() { cat > temp; $TEST_DIR/../main.exe format temp; }

TOPLEVEL DEFINITIONS

  $ main <<\.
  > a = 0
  > .
  a = 0

  $ main <<\.
  > a = 0,
  > b = 0
  > .
  a = 0,
  b = 0

STRINGS

Chars and strings support various escape sequences and also interpolated expressions..

  $ main <<\.
  > a = "abc \n\t {0} {x} {let x=0,x}",
  > b = "this is a test {f(a)(b)}",
  > c = "\{}",
  > d = "'  \""
  > .
  a = "abc \n\t {0} {x} {let x = 0,
  x}",
  b = "this is a test {f a b}",
  c = "\{}",
  d = "\'  \""

CHARS

  $ main <<\.
  > a = 'a',
  > b = '\n',
  > c = '"',
  > d = '\''
  > .
  a = 'a',
  b = '\n',
  c = '\"',
  d = '\''

LET EXPRESSIONS

Let-bindings are multiline and thus go do not stay on the same line as a definition.

  $ main <<\.
  > a = let b = 0, b
  > .
  a =
    let b = 0,
    b

Multiple let bindings are composable in multiple directions; the bound
expression gets indented if it is multiline, but the body expression does not.

  $ main <<\.
  > a = let b = let c = let d = 0, d, c, let e = let f = f, e, e
  > .
  a =
    let b =
      let c =
        let d = 0,
        d,
      c,
    let e =
      let f = f,
      e,
    e

FUNCTIONS

Single line functions go on the same line as the definition, and multiline
functions go on the next line.

  $ main <<\.
  > a = fun x: x,
  > b = fun x: fun y: x,
  > c = fun x: let y = 0, y,
  > d = fun w x: fun y: let z = 0, z
  > .
  a = fun x: x,
  b = fun x: fun y: x,
  c =
    fun x:
      let y = 0,
      y,
  d =
    fun w x:
      fun y:
        let z = 0,
        z

CALLS

Calls generally remain on the same line, except insofar as the sub-expressions
have multiple lines.

  $ main <<\.
  > a = map xs (fun x: add x 1),
  > b = map xs (fun x: let y = add x 1, y),
  > c = f (let x = a, x) (let x = b, x),
  > d = f (g a b) (h c (i d e f))
  > .
  a = map xs (fun x: add x 1),
  b =
    map xs (fun x:
      let y = add x 1,
      y),
  c =
    f (let x = a,
    x) (let x = b,
    x),
  d = f (g a b) (h c (i d e f))

MATCH EXPRESSIONS

Match expressions are similar to let expressions in that they can be composed
in two directions. Parentheses are often necessary to make this possible.

  $ main <<\.
  > a = match x|A:0|B:1|C:2,
  > b = match(match x|A:0)|0:(match x|A:1) |_:2,
  > c = match x| A b c: "abc"| x: "def"
  > .
  a =
    match x
    | A: 0
    | B: 1
    | C: 2,
  b =
    match
      (match x
       | A: 0)
    | 0:
      (match x
       | A: 1)
    | _: 2,
  c =
    match x
    | A b c: "abc"
    | x: "def"

SEQUENCE EXPRESSIONS

Two expressions can be sequenced. The formatted version may re-associate the
composition, since this is a valid transformation.

  $ main <<\.
  > a = b; (c; d); e,
  > b = let x = 0, print x; let y = 1, print y
  > .
  a =
    b;
    c;
    d;
    e,
  b =
    let x = 0,
    print x;
    let y = 1,
    print y
