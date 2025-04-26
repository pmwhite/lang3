(*
   Rules:
     - Don't plan ahead; just go!
     - It doesn't matter what things are next to other things; just put stuff
     in the right dependency order (because you have to).
*)

open struct
  [@@@warning "-32-60"]

  let ( <> ) = `Shadowed
  let ( = ) = `Shadowed
  let ( < ) : int -> int -> bool = ( < )
  let ( <= ) : int -> int -> bool = ( <= )

  module List = ListLabels
  module Array = ArrayLabels
  module Bytes = BytesLabels
  module String = StringLabels
end

let printfn fmt = Printf.ksprintf print_endline fmt

let is_symbol_char c =
  match c with
  | 'a' .. 'z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let symbol_of_string s =
  match s with
  | "fun" -> `Keyword_fun
  | "let" -> `Keyword_let
  | _ -> `Name s
;;

let rec parse_rest_of_symbol s i len buf =
  if i < len
  then (
    let c = s.[i] in
    if is_symbol_char c
    then (
      Buffer.add_char buf c;
      parse_rest_of_symbol s (i + 1) len buf)
    else i, symbol_of_string (Buffer.contents buf))
  else i, symbol_of_string (Buffer.contents buf)
;;

let errorfn fmt =
  (* TODO: Add locations here. *)
  Printf.ksprintf
    (fun s ->
       Printf.eprintf "%s\n" s;
       exit 1)
    fmt
;;

let rec parse_rest_of_integer s i len acc =
  if i < len
  then (
    let c = s.[i] in
    match c with
    | '0' .. '9' ->
      let digit = Char.code c - Char.code '0' in
      let acc = (acc * 10) + digit in
      if acc >= 0
      then parse_rest_of_integer s (i + 1) len acc
      else
        errorfn "Integer literal must be representable in 31 bits, but it is too large."
    | _ -> i, acc)
  else i, acc
;;

let rec parse_rest_of_string s i len buf =
  if i < len
  then (
    let c = s.[i] in
    match c with
    | '"' -> i + 1, Buffer.contents buf
    | _ ->
      Buffer.add_char buf c;
      parse_rest_of_string s (i + 1) len buf)
  else errorfn "String literal left unfinished '\"'"
;;

type expr =
  | Name of string
  | Integer of int
  | String of string
  | Fun of string list * expr
  | Let of string * expr * expr
  | Call of expr * expr list

let rec skip_whitespace s i len =
  if i < len
  then (
    let c = s.[i] in
    match c with
    | ' ' | '\t' | '\n' -> skip_whitespace s (i + 1) len
    | _ -> i)
  else i
;;

let rec parse_args s i len acc =
  if i < len
  then (
    let c = s.[i] in
    match c with
    | 'a' .. 'z' ->
      let buf = Buffer.create 128 in
      Buffer.add_char buf c;
      let i, symbol = parse_rest_of_symbol s (i + 1) len buf in
      (match symbol with
       | `Name name ->
         let i = skip_whitespace s i len in
         parse_args s i len (name :: acc)
       | `Keyword_fun -> errorfn "Expected name, but got keyword 'fun'."
       | `Keyword_let -> errorfn "Expected name, but got keyword 'let'.")
    | ':' -> i, List.rev acc
    | _ -> errorfn "Expected name, but found unexpected character '%c'." c)
  else errorfn "Expected name, but the program ended."
;;

let factors_to_expr factors =
  match factors with
  | [] -> errorfn "BUG: Empty list of factors."
  | [ factor ] -> factor
  | fun_ :: (_ :: _ as args) -> Call (fun_, args)
;;

let rec parse_factor s i len =
  if i < len
  then (
    let c = s.[i] in
    match c with
    | 'a' .. 'z' ->
      let buf = Buffer.create 128 in
      Buffer.add_char buf c;
      let i, symbol = parse_rest_of_symbol s (i + 1) len buf in
      (match symbol with
       | `Name name -> i, Name name
       | `Keyword_fun ->
         let i = skip_whitespace s i len in
         let i, args = parse_args s i len [] in
         let i = skip_whitespace s i len in
         let i, body = parse_expr s i len [] in
         i, Fun (args, body)
       | `Keyword_let ->
         let i = skip_whitespace s i len in
         assert false)
    | '0' .. '9' ->
      let initial = Char.code c - Char.code '0' in
      let i, integer = parse_rest_of_integer s (i + 1) len initial in
      i, Integer integer
    | '"' ->
      let buf = Buffer.create 128 in
      let i, string = parse_rest_of_string s (i + 1) len buf in
      i, String string
    | _ ->
      errorfn
        "Expected to find the beginning of an expression, but found an unexpected \
         character '%c' instead."
        c)
  else errorfn "Expected to find an expression, but the program ended."

and parse_expr s i len acc =
  let i, factor = parse_factor s i len in
  let i = skip_whitespace s i len in
  if i < len
  then (
    let c = s.[i] in
    match c with
    | ')' -> i + 1, factors_to_expr (List.rev acc)
    | _ -> parse_expr s i len (factor :: acc))
  else i, factors_to_expr (List.rev acc)
;;

let parse_program s =
  let len = String.length s in
  if Int.equal len 0
  then errorfn "Program must consist of text, but it is empty."
  else parse_factor s 0 len
;;

let () =
  match Array.length Sys.argv with
  | 0 | 1 ->
    printfn "Usage: ./main.exe FILE";
    printfn "A program interpreter."
  | 2 -> printfn "Running '%s'" Sys.argv.(1)
  | _ ->
    printfn "Too many arguments. Usage: ./main.exe FILE";
    exit 1
;;
