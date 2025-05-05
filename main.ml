(*
   Rules:
     - Don't plan ahead; just go!
     - It doesn't matter what things are next to other things; just put stuff
     in the right dependency order (because you have to).

   Todo:
     - format strings
     - add locations to expressions
     - unescape stuff when formatting
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

module String_map = Map.Make (String)

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

let rec parse_rest_of_string s i len closer syntax_buf value_buf =
  if i < len
  then (
    let c = s.[i] in
    match c with
    | _ when Char.equal c closer ->
      i + 1, Buffer.contents syntax_buf, Buffer.contents value_buf
    | '\\' ->
      Buffer.add_char syntax_buf '\\';
      let i = i + 1 in
      if i < len
      then (
        let c = s.[i] in
        Buffer.add_char syntax_buf c;
        let value_char =
          match c with
          | 't' -> '\t'
          | 'n' -> '\n'
          | '\\' -> '\\'
          | '{' -> '{'
          | '"' -> '"'
          | '\'' -> '\''
          | _ -> errorfn (Loc (s, i)) "Expected an escapable character, but found '%c'." c
        in
        Buffer.add_char value_buf value_char;
        parse_rest_of_string s (i + 1) len closer syntax_buf value_buf)
      else
        errorfn
          (Loc (s, i))
          "Expected an escapable character, but reached end of program."
    | _ ->
      Buffer.add_char syntax_buf c;
      Buffer.add_char value_buf c;
      parse_rest_of_string s (i + 1) len closer syntax_buf value_buf)
  else errorfn (Loc (s, i)) "String literal left unfinished '\"'"
;;

type expr =
  | Wildcard of loc
  | Name of string * loc
  | Data of string * loc
  | Integer of int * loc
  | String of string * string * loc
  | Char of string * char * loc
  | Fun of expr list * expr * loc
  | Let of expr * expr * expr * loc
  | Seq of expr * expr * loc
  | Call of expr * expr list * loc
  | Match of expr * (expr list * expr) list * loc

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

let factors_to_expr factors loc =
  match factors with
  | [] -> errorfn loc "BUG: Empty list of factors."
  | [ factor ] -> factor
  | fun_ :: (_ :: _ as args) -> Call (fun_, args, loc)
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
    let loc = Loc (s, i) in
    let c = s.[i] in
    match c with
    | 'a' .. 'z' ->
      let i, symbol = parse_symbol_assume_first_char s i len in
      (match symbol with
       | `Name name -> i, Name (name, loc)
       | `Keyword_fun ->
         let i = skip_whitespace s i len in
         let i, args = parse_factor_sequence s i len [] in
         let i = skip_whitespace s i len in
         let i = skip_exact_char s i len ':' in
         let i = skip_whitespace s i len in
         let i, body = parse_expr s i len in
         i, Fun (args, body, loc)
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
         i, Let (pattern, expr, body, loc)
       | `Keyword_match ->
         let i = skip_whitespace s i len in
         let i, expr = parse_expr s i len in
         let i = skip_whitespace s i len in
         let i, cases = parse_cases s i len [] in
         i, Match (expr, cases, loc))
    | 'A' .. 'Z' ->
      let buf = Buffer.create 128 in
      Buffer.add_char buf c;
      let i, data_name = parse_rest_of_data_name s (i + 1) len buf in
      i, Data (data_name, loc)
    | '0' .. '9' ->
      let initial = Char.code c - Char.code '0' in
      let i, integer = parse_rest_of_integer s (i + 1) len initial in
      i, Integer (integer, loc)
    | '"' ->
      let syntax_buf = Buffer.create 128 in
      let value_buf = Buffer.create 128 in
      let i, syntax, value =
        parse_rest_of_string s (i + 1) len '"' syntax_buf value_buf
      in
      i, String (syntax, value, loc)
    | '\'' ->
      let syntax_buf = Buffer.create 128 in
      let value_buf = Buffer.create 128 in
      let i, syntax, value =
        parse_rest_of_string s (i + 1) len '\'' syntax_buf value_buf
      in
      (match String.length value with
       | 1 -> i, Char (syntax, value.[0], loc)
       | _ ->
         errorfn (Loc (s, i)) "Character literal must only describe single character.")
    | '(' ->
      let i, expr = parse_expr s (i + 1) len in
      let i = skip_exact_char s i len ')' in
      i, expr
    | '_' -> i + 1, Wildcard loc
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
  let loc = Loc (s, i) in
  let i, factors = parse_factor_sequence s i len [] in
  let expr = factors_to_expr factors loc in
  let i = skip_whitespace s i len in
  if i < len
  then (
    let c = s.[i] in
    match c with
    | ';' ->
      let i = i + 1 in
      let i = skip_whitespace s i len in
      let i, next = parse_expr s i len in
      i, Seq (expr, next, loc)
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

let rec format_indent buf indent =
  if indent > 0
  then (
    Buffer.add_char buf ' ';
    format_indent buf (indent - 1))
  else ()
;;

let rec is_multiline_expr expr =
  match expr with
  | Wildcard _ | Name _ | Integer _ | String _ | Char _ | Data _ -> false
  | Let _ | Seq _ | Match _ -> true
  | Fun (_, body, _) -> is_multiline_expr body
  | Call (fun_, args, _) ->
    is_multiline_expr fun_ || List.exists args ~f:is_multiline_expr
;;

let space_or_newline_and_indent buf indent expr =
  if is_multiline_expr expr
  then (
    let indent = indent + 2 in
    Buffer.add_char buf '\n';
    format_indent buf indent;
    indent)
  else (
    Buffer.add_char buf ' ';
    indent)
;;

let rec format_expr buf indent parent expr =
  match expr with
  | Wildcard _ -> Buffer.add_string buf "_"
  | Name (name, _) -> Buffer.add_string buf name
  | Integer (i, _) -> Buffer.add_string buf (Int.to_string i)
  | String (syntax, _value, _) ->
    Buffer.add_char buf '"';
    Buffer.add_string buf syntax;
    Buffer.add_char buf '"'
  | Char (syntax, _c, _) ->
    Buffer.add_char buf '\'';
    Buffer.add_string buf syntax;
    Buffer.add_char buf '\''
  | Fun (args, body, _) ->
    Buffer.add_string buf "fun";
    List.iter args ~f:(fun arg ->
      Buffer.add_char buf ' ';
      format_factor buf indent `Non_match arg);
    Buffer.add_string buf ":";
    let indent = space_or_newline_and_indent buf indent expr in
    format_expr buf indent `Non_match body
  | Let (pattern, expr, body, _) ->
    Buffer.add_string buf "let ";
    format_expr buf indent `Non_match pattern;
    Buffer.add_string buf " = ";
    format_expr buf indent `Non_match expr;
    Buffer.add_string buf ",\n";
    format_indent buf indent;
    format_expr buf indent parent body
  | Seq (expr, next, _) ->
    format_expr buf indent `Non_match expr;
    Buffer.add_string buf ";\n";
    format_indent buf indent;
    format_expr buf indent `Non_match next
  | Call (fun_, args, _) ->
    format_factor buf indent `Non_match fun_;
    List.iter args ~f:(fun arg ->
      Buffer.add_char buf ' ';
      format_factor buf indent `Non_match arg)
  | Data (name, _) -> Buffer.add_string buf name
  | Match (expr, cases, _) ->
    let indent =
      match parent with
      | `Match ->
        Buffer.add_char buf '(';
        indent + 1
      | `Non_match -> indent
    in
    Buffer.add_string buf "match ";
    format_expr buf indent `Match expr;
    List.iter cases ~f:(fun (patterns, body) ->
      Buffer.add_char buf '\n';
      format_indent buf indent;
      (match patterns with
       | hd :: tl ->
         Buffer.add_string buf "| ";
         format_expr buf indent `Match hd;
         List.iter tl ~f:(fun pattern ->
           Buffer.add_string buf " | ";
           format_expr buf indent `Match pattern)
       | [] -> errorfn Noloc "BUG: no patterns in case");
      Buffer.add_string buf ":";
      let indent = space_or_newline_and_indent buf indent body in
      format_expr buf indent `Match body);
    (match parent with
     | `Match -> Buffer.add_char buf ')'
     | `Non_match -> ())

and format_factor buf indent parent expr =
  match expr with
  | Wildcard _ | Name _ | Data (_, _) | Integer _ | String _ | Char _ ->
    format_expr buf indent parent expr
  | Fun _ | Let _ | Seq _ | Call _ | Match _ ->
    Buffer.add_char buf '(';
    format_expr buf indent `Non_match expr;
    Buffer.add_char buf ')'
;;

type value =
  | Vdata of string
  | Vinteger of int
  | Vstring of string
  | Vchar of char
  | Vfun of expr list * expr
  | Vcall of value * value list

let rec value_to_expr value =
  match value with
  | Vdata name -> Data (name, Noloc)
  | Vinteger i -> Integer (i, Noloc)
  | Vstring s -> String (s, s, Noloc)
  | Vchar c -> Char (Printf.sprintf "%c" c, c, Noloc)
  | Vfun (args, body) -> Fun (args, body, Noloc)
  | Vcall (name, args) -> Call (value_to_expr name, List.map args ~f:value_to_expr, Noloc)
;;

let rec evaluate_expr context expr =
  match expr with
  | Wildcard loc -> errorfn loc "ABORT: Wildcard expression used as value."
  | Name (name, loc) ->
    (match String_map.find_opt name context with
     | None -> errorfn loc "ABORT: Name '%s' is not defined." name
     | Some value -> value)
  | Data (name, _) -> Vdata name
  | Integer (i, _) -> Vinteger i
  | String (_syntax, value, _) -> Vstring value
  | Char (_syntax, value, _) -> Vchar value
  | Fun (args, body, _) -> Vfun (args, body)
  | Let (pattern, expr, body, _) -> evaluate_match context expr [ [ pattern ], body ]
  | Seq (a, b, loc) -> evaluate_match context a [ [ Data ("T", loc) ], b ]
  | Match (expr, cases, _) -> evaluate_match context expr cases
  | Call (fun_, args, loc) ->
    let fun_ = evaluate_expr context fun_ in
    let args = List.map args ~f:(fun arg -> evaluate_expr context arg) in
    (match fun_ with
     | Vdata _ -> Vcall (fun_, args)
     | Vinteger _ -> errorfn loc "ABORT: Attempted to call an integer value."
     | Vstring _ -> errorfn loc "ABORT: Attempted to call a string value."
     | Vchar _ -> errorfn loc "ABORT: Attempted to call a char value."
     | Vcall _ -> errorfn loc "ABORT: Attempted to call a call value."
     | Vfun (arg_patterns, body) -> evaluate_call context arg_patterns args body)

and evaluate_call context arg_patterns args body =
  match arg_patterns with
  | [] ->
    (match args with
     | [] -> evaluate_expr context body
     | _ :: _ -> errorfn Noloc "ABORT: Too many arguments.")
  | arg_pattern :: arg_patterns ->
    (match args with
     | [] -> errorfn Noloc "ABORT: Not enough args."
     | arg :: args ->
       (match evaluate_pattern context arg_pattern arg with
        | None -> errorfn Noloc "ABORT: No patterns matched value."
        | Some context -> evaluate_call context arg_patterns args body))

and evaluate_match context expr cases =
  let value = evaluate_expr context expr in
  match
    List.find_map cases ~f:(fun (patterns, body) ->
      match
        List.find_map patterns ~f:(fun pattern -> evaluate_pattern context pattern value)
      with
      | None -> None
      | Some context -> Some (evaluate_expr context body))
  with
  | None -> errorfn Noloc "ABORT: No patterns matched value."
  | Some value -> value

and evaluate_call_patterns context arg_patterns args =
  match arg_patterns with
  | [] ->
    (match args with
     | [] -> Some context
     | _ :: _ -> None)
  | arg_pattern :: arg_patterns ->
    (match args with
     | [] -> None
     | arg :: args ->
       (match evaluate_pattern context arg_pattern arg with
        | None -> None
        | Some context -> evaluate_call_patterns context arg_patterns args))

and evaluate_pattern context pattern value =
  match pattern with
  | Wildcard _ -> Some context
  | Name (name, _) -> Some (String_map.add name value context)
  | Data (name, _) ->
    (match value with
     | Vdata vname -> if String.equal name vname then None else Some context
     | Vinteger _ | Vstring _ | Vchar _ | Vfun _ | Vcall _ -> None)
  | Integer (i, _) ->
    (match value with
     | Vinteger vi -> if Int.equal i vi then None else Some context
     | Vdata _ | Vstring _ | Vchar _ | Vfun _ | Vcall _ -> None)
  | String (_syntax, string, _) ->
    (match value with
     | Vstring vstring -> if String.equal string vstring then None else Some context
     | Vdata _ | Vinteger _ | Vchar _ | Vfun _ | Vcall _ -> None)
  | Char (_syntax, c, _) ->
    (match value with
     | Vchar vc -> if Char.equal c vc then None else Some context
     | Vdata _ | Vinteger _ | Vstring _ | Vfun _ | Vcall _ -> None)
  | Fun (_, _, loc) ->
    errorfn loc "ABORT: Attempted to use function expression as pattern."
  | Let (_, _, _, loc) -> errorfn loc "ABORT: Attempted to use let expression as pattern."
  | Seq (_, _, loc) ->
    errorfn loc "ABORT: Attempted to use a sequence of two expressions as pattern."
  | Call (fun_, args, _) ->
    (match value with
     | Vcall (vfun_, vargs) ->
       evaluate_call_patterns context (fun_ :: args) (vfun_ :: vargs)
     | Vdata _ | Vinteger _ | Vchar _ | Vstring _ | Vfun _ -> None)
  | Match (_, _, loc) ->
    errorfn loc "ABORT: Attempted to use match expression as pattern."
;;

let print_help () =
  printfn "Commands:";
  printfn "    check FILE            Ensure the validity of syntax in a file";
  printfn "    format FILE           Print the formatted form of code in a file";
  printfn "    format-in-place FILE  Modify a file's contents to be formatted";
  printfn "    run FILE              Run a file";
  printfn "    repl                  Start an interactive interpreter session"
;;

let () =
  let num_args = Array.length Sys.argv in
  if num_args < 2
  then (
    printfn "The Alpaca programming language.";
    print_help ())
  else (
    let command = Sys.argv.(1) in
    match command with
    | "format" ->
      (match num_args with
       | 0 | 1 | 2 ->
         printfn "Not enough arguments";
         print_help ()
       | 3 ->
         let filename = Sys.argv.(2) in
         let contents = In_channel.with_open_bin filename In_channel.input_all in
         let parsed = parse_program contents in
         let buf = Buffer.create 1024 in
         format_expr buf 0 `Non_match parsed;
         printfn "%s" (Buffer.contents buf)
       | _ ->
         printfn "Too many arguments";
         print_help ())
    | "repl" ->
      (match num_args with
       | 0 | 1 ->
         printfn "Not enough arguments";
         print_help ()
       | 2 ->
         while true do
           Printf.printf "> %!";
           let line = read_line () in
           let parsed = parse_program line in
           let value = evaluate_expr String_map.empty parsed in
           let buf = Buffer.create 1024 in
           format_expr buf 0 `Non_match (value_to_expr value);
           printfn "%s" (Buffer.contents buf)
         done
       | _ ->
         printfn "Too many arguments";
         print_help ())
    | "run" ->
      (match num_args with
       | 0 | 1 | 2 ->
         printfn "Not enough arguments";
         print_help ()
       | 3 ->
         let filename = Sys.argv.(2) in
         let contents = In_channel.with_open_bin filename In_channel.input_all in
         let parsed = parse_program contents in
         let value = evaluate_expr String_map.empty parsed in
         let buf = Buffer.create 1024 in
         format_expr buf 0 `Non_match (value_to_expr value);
         printfn "%s" (Buffer.contents buf)
       | _ ->
         printfn "Too many arguments";
         print_help ())
    | _ -> errorfn Noloc "'%s' is not a recognized command." command)
;;
