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
let dbgfn fmt = Printf.ksprintf print_endline fmt

let is_symbol_char c =
  match c with
  | 'a' .. 'z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let symbol_of_string s =
  match s with
  | "fun" -> `Keyword_fun
  | "let" -> `Keyword_let
  | "match" -> `Keyword_match
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

let rec parse_rest_of_data_name s i len buf =
  if i < len
  then (
    let c = s.[i] in
    if is_symbol_char c
    then (
      Buffer.add_char buf c;
      parse_rest_of_data_name s (i + 1) len buf)
    else i, Buffer.contents buf)
  else i, Buffer.contents buf
;;

let rec line_and_column s acc_l acc_i goal_i =
  match String.index_from_opt s acc_i '\n' with
  | None -> acc_l, goal_i - acc_i + 1
  | Some i ->
    if i < goal_i
    then line_and_column s (acc_l + 1) (i + 1) goal_i
    else acc_l, goal_i - acc_i + 1
;;

type loc =
  | Noloc
  | Loc of string * int

let errorfn loc fmt =
  let loc =
    match loc with
    | Noloc -> ""
    | Loc (s, i) ->
      let line, column = line_and_column s 1 0 i in
      Printf.sprintf "%d:%d " line column
  in
  Printf.ksprintf
    (fun s ->
       Printf.eprintf "%s%s\n" loc s;
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
        errorfn
          (Loc (s, i))
          "Integer literal must be representable in 31 bits, but it is too large."
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
  else errorfn (Loc (s, i)) "String literal left unfinished '\"'"
;;

type expr =
  | Wildcard
  | Name of string
  | Data of string
  | Integer of int
  | String of string
  | Fun of expr list * expr
  | Let of expr * expr * expr
  | Seq of expr * expr
  | Call of expr * expr list
  | Match of expr * (expr list * expr) list

let rec skip_whitespace s i len =
  if i < len
  then (
    let c = s.[i] in
    match c with
    | ' ' | '\t' | '\n' -> skip_whitespace s (i + 1) len
    | _ -> i)
  else i
;;

let parse_symbol_assume_first_char s i len =
  let buf = Buffer.create 128 in
  Buffer.add_char buf s.[i];
  parse_rest_of_symbol s (i + 1) len buf
;;

let parse_symbol s i len =
  if i < len
  then (
    let c = s.[i] in
    match c with
    | 'a' .. 'z' -> parse_symbol_assume_first_char s i len
    | _ -> errorfn (Loc (s, i)) "Expected symbol, but found non-symbol character '%c'." c)
  else errorfn (Loc (s, i)) "Expected symbol, but reached end of program."
;;

let rec parse_args s i len acc =
  if i < len
  then (
    let c = s.[i] in
    match c with
    | 'a' .. 'z' ->
      let i, symbol = parse_symbol_assume_first_char s i len in
      (match symbol with
       | `Name name ->
         let i = skip_whitespace s i len in
         parse_args s i len (name :: acc)
       | `Keyword_fun -> errorfn (Loc (s, i)) "Expected name, but got keyword 'fun'."
       | `Keyword_let -> errorfn (Loc (s, i)) "Expected name, but got keyword 'let'."
       | `Keyword_match -> errorfn (Loc (s, i)) "Expected name, but got keyword 'match.")
    | ':' -> i, List.rev acc
    | _ -> errorfn (Loc (s, i)) "Expected name, but found unexpected character '%c'." c)
  else errorfn (Loc (s, i)) "Expected name, but the program ended."
;;

let factors_to_expr factors =
  match factors with
  | [] -> errorfn Noloc "BUG: Empty list of factors."
  | [ factor ] -> factor
  | fun_ :: (_ :: _ as args) -> Call (fun_, args)
;;

let skip_exact_char s i len char =
  if i < len
  then (
    let c = s.[i] in
    if Char.equal char c
    then i + 1
    else errorfn (Loc (s, i)) "Expected '%c', but found '%c'" char c)
  else errorfn (Loc (s, i)) "Expected '%c', but reached end of program." char
;;

let rec parse_factor s i len =
  if i < len
  then (
    let c = s.[i] in
    match c with
    | 'a' .. 'z' ->
      let i, symbol = parse_symbol_assume_first_char s i len in
      (match symbol with
       | `Name name -> i, Name name
       | `Keyword_fun ->
         let i = skip_whitespace s i len in
         let i, args = parse_factor_sequence s i len [] in
         let i = skip_whitespace s i len in
         let i = skip_exact_char s i len ':' in
         let i = skip_whitespace s i len in
         let i, body = parse_expr s i len in
         i, Fun (args, body)
       | `Keyword_let ->
         let i = skip_whitespace s i len in
         let i, pattern = parse_expr s i len in
         let i = skip_whitespace s i len in
         let i = skip_exact_char s i len '=' in
         let i = skip_whitespace s i len in
         let i, expr = parse_expr s i len in
         let i = skip_whitespace s i len in
         let i = skip_exact_char s i len ',' in
         let i = skip_whitespace s i len in
         let i, body = parse_expr s i len in
         i, Let (pattern, expr, body)
       | `Keyword_match ->
         let i = skip_whitespace s i len in
         let i, expr = parse_expr s i len in
         let i = skip_whitespace s i len in
         let i, cases = parse_cases s i len [] in
         i, Match (expr, cases))
    | 'A' .. 'Z' ->
      let buf = Buffer.create 128 in
      Buffer.add_char buf c;
      let i, data_name = parse_rest_of_data_name s (i + 1) len buf in
      i, Data data_name
    | '0' .. '9' ->
      let initial = Char.code c - Char.code '0' in
      let i, integer = parse_rest_of_integer s (i + 1) len initial in
      i, Integer integer
    | '"' ->
      let buf = Buffer.create 128 in
      let i, string = parse_rest_of_string s (i + 1) len buf in
      i, String string
    | '(' ->
      let i, expr = parse_expr s (i + 1) len in
      let i = skip_exact_char s i len ')' in
      i, expr
    | '_' -> i + 1, Wildcard
    | _ ->
      errorfn
        (Loc (s, i))
        "Expected to find the beginning of an expression, but found an unexpected \
         character '%c' instead."
        c)
  else errorfn (Loc (s, i)) "Expected to find an expression, but the program ended."

and parse_patterns s i len acc =
  if i < len
  then (
    let c = s.[i] in
    match c with
    | '|' ->
      let i = i + 1 in
      let i = skip_whitespace s i len in
      let i, pattern = parse_expr s i len in
      let i = skip_whitespace s i len in
      parse_patterns s i len (pattern :: acc)
    | ':' -> i + 1, List.rev acc
    | _ -> errorfn (Loc (s, i)) "Expected either '|' or ':', but got character '%c'" c)
  else errorfn (Loc (s, i)) "Expected either '|' or ':', but reached end of program."

and parse_cases s i len acc =
  if i < len
  then (
    let c = s.[i] in
    match c with
    | '|' ->
      let i, patterns = parse_patterns s i len [] in
      let i = skip_whitespace s i len in
      let i, body = parse_expr s i len in
      parse_cases s i len ((patterns, body) :: acc)
    | _ -> i, List.rev acc)
  else i, List.rev acc

and parse_factor_sequence s i len acc =
  let i, factor = parse_factor s i len in
  let acc = factor :: acc in
  let i = skip_whitespace s i len in
  if i < len
  then (
    let c = s.[i] in
    match c with
    | ')' | '=' | ';' | ':' | '|' | ',' -> i, List.rev acc
    | _ -> parse_factor_sequence s i len acc)
  else i, List.rev acc

and parse_expr s i len =
  let i, factors = parse_factor_sequence s i len [] in
  let expr = factors_to_expr factors in
  let i = skip_whitespace s i len in
  if i < len
  then (
    let c = s.[i] in
    match c with
    | ';' ->
      let i = i + 1 in
      let i = skip_whitespace s i len in
      let i, next = parse_expr s i len in
      i, Seq (expr, next)
    | _ -> i, expr)
  else i, expr
;;

let parse_program s =
  let len = String.length s in
  let i, expr = parse_expr s 0 len in
  if i < len
  then
    errorfn
      (Loc (s, i))
      "Finished parsing program before the end of the input text was reached."
  else expr
;;

let rec format_expr buf expr =
  match expr with
  | Wildcard -> Buffer.add_string buf "_"
  | Name name -> Buffer.add_string buf name
  | Integer i -> Buffer.add_string buf (Int.to_string i)
  | String s ->
    Buffer.add_char buf '"';
    Buffer.add_string buf s;
    Buffer.add_char buf '"'
  | Fun (args, body) ->
    (match args with
     | [] -> ()
     | arg :: args ->
       format_factor buf arg;
       List.iter args ~f:(fun arg ->
         Buffer.add_char buf ' ';
         format_factor buf arg);
       Buffer.add_string buf ": ");
    format_expr buf body
  | Let (pattern, expr, body) ->
    Buffer.add_string buf "let ";
    format_expr buf pattern;
    Buffer.add_string buf " = ";
    format_expr buf expr;
    Buffer.add_string buf ";\n";
    format_expr buf body
  | Seq (expr, next) ->
    format_expr buf expr;
    Buffer.add_string buf ";\n";
    format_expr buf next
  | Call (fun_, args) ->
    format_factor buf fun_;
    List.iter args ~f:(fun arg ->
      Buffer.add_char buf ' ';
      format_factor buf arg)
  | Data name -> Buffer.add_string buf name
  | Match (expr, cases) ->
    Buffer.add_string buf "match ";
    format_expr buf expr;
    List.iter cases ~f:(fun (patterns, body) ->
      List.iter patterns ~f:(fun pattern ->
        Buffer.add_string buf "| ";
        format_expr buf pattern);
      Buffer.add_string buf ": ";
      format_expr buf body)

and format_factor buf expr =
  match expr with
  | Wildcard | Name _ | Data _ | Integer _ | String _ -> format_expr buf expr
  | Fun _ | Let _ | Seq _ | Call _ | Match _ ->
    Buffer.add_char buf '(';
    format_expr buf expr;
    Buffer.add_char buf ')'
;;

let () =
  match Array.length Sys.argv with
  | 0 | 1 ->
    printfn "Usage: ./main.exe FILE";
    printfn "A program interpreter."
  | 2 ->
    let filename = Sys.argv.(1) in
    let contents = In_channel.with_open_bin filename In_channel.input_all in
    let parsed = parse_program contents in
    let buf = Buffer.create 1024 in
    let () = format_expr buf parsed in
    printfn "%s" (Buffer.contents buf)
  | _ -> errorfn Noloc "Too many arguments. Usage: ./main.exe FILE"
;;
