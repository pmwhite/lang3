(*
   Rules:
     - Don't plan ahead; just go!
     - It doesn't matter what things are next to other things; just put stuff
     in the right dependency order (because you have to).

   Todo:
     - location ranges
     - namespacing and toplevel declarations
     - program introspection commands
     - ai edit loop wrapper
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

let rec line_and_column s acc_l prev_acc_i acc_i goal_i =
  match String.index_from_opt s acc_i '\n' with
  | None ->
    let excerpt = String.sub ~pos:prev_acc_i ~len:(String.length s - prev_acc_i) s in
    let line = String.sub ~pos:acc_i ~len:(String.length s - acc_i) s in
    acc_l, goal_i - acc_i + 1, excerpt, line
  | Some i ->
    if i < goal_i
    then line_and_column s (acc_l + 1) acc_i (i + 1) goal_i
    else (
      let excerpt = String.sub ~pos:prev_acc_i ~len:(i - prev_acc_i) s in
      let line = String.sub ~pos:acc_i ~len:(i - acc_i) s in
      acc_l, goal_i - acc_i + 1, excerpt, line)
;;

type loc =
  | Noloc
  | Loc of string * int

let errorfn loc fmt =
  match loc with
  | Noloc ->
    Printf.ksprintf
      (fun s ->
         Printf.eprintf "%s\n" s;
         exit 1)
      fmt
  | Loc (s, i) ->
    let lnum, cnum, excerpt, line = line_and_column s 1 0 0 i in
    let excerpt_lines = String.split_on_char ~sep:'\n' excerpt in
    Printf.ksprintf
      (fun s ->
         Printf.eprintf "%d:%d %s\n" lnum cnum s;
         List.iter excerpt_lines ~f:(fun line -> Printf.eprintf "| %s\n" line);
         let indent = String.make cnum '-' in
         Printf.eprintf "\\%s^\n" indent;
         let begin_ = max (cnum - 4) 0 in
         let end_ = min (cnum + 4) (String.length line) in
         Printf.eprintf
           "Specifically this part: `%s`\n"
           (String.sub line ~pos:begin_ ~len:(end_ - begin_));
         exit 1)
      fmt
;;

let abortfn loc fmt = Printf.ksprintf (errorfn loc "ABORT: %s") fmt

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

let parse_string_section s i len closer =
  let rec iter s i len closer buf =
    if i < len
    then (
      let c = s.[i] in
      match c with
      | _ when Char.equal c closer -> i + 1, Buffer.contents buf, `Done
      | '{' -> i + 1, Buffer.contents buf, `Keep_going
      | '\\' ->
        let i = i + 1 in
        if i < len
        then (
          let c = s.[i] in
          let value_char =
            match c with
            | 't' -> '\t'
            | 'n' -> '\n'
            | '\\' -> '\\'
            | '{' -> '{'
            | '"' -> '"'
            | '\'' -> '\''
            | _ ->
              errorfn (Loc (s, i)) "Expected an escapable character, but found '%c'." c
          in
          Buffer.add_char buf value_char;
          iter s (i + 1) len closer buf)
        else
          errorfn
            (Loc (s, i))
            "Expected an escapable character, but reached end of program."
      | _ ->
        Buffer.add_char buf c;
        iter s (i + 1) len closer buf)
    else errorfn (Loc (s, i)) "String literal left unfinished '\"'"
  in
  let buf = Buffer.create 128 in
  iter s i len closer buf
;;

type expr =
  | Wildcard of loc
  | Name of string * loc
  | Data of string * loc
  | Integer of int * loc
  | String of string * (expr * string) list * loc
  | Char of char * loc
  | Fun of expr list * expr * loc
  | Let of expr * expr * expr * loc
  | Seq of expr * expr * loc
  | Call of expr * expr list * loc
  | Match of expr * (expr list * expr) list * loc

type program = (string * expr) list

let loc_of_expr = function
  | Wildcard loc
  | Name (_, loc)
  | Data (_, loc)
  | Integer (_, loc)
  | String (_, _, loc)
  | Char (_, loc)
  | Fun (_, _, loc)
  | Let (_, _, _, loc)
  | Seq (_, _, loc)
  | Call (_, _, loc)
  | Match (_, _, loc) -> loc
;;

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

let expect_name_of_symbol s i symbol =
  match symbol with
  | `Name name -> name
  | `Keyword_fun -> errorfn (Loc (s, i)) "Expected name, but got keyword 'fun'."
  | `Keyword_let -> errorfn (Loc (s, i)) "Expected name, but got keyword 'let'."
  | `Keyword_match -> errorfn (Loc (s, i)) "Expected name, but got keyword 'match."
;;

let rec parse_args s i len acc =
  if i < len
  then (
    let c = s.[i] in
    match c with
    | 'a' .. 'z' ->
      let i, symbol = parse_symbol_assume_first_char s i len in
      let name = expect_name_of_symbol s i symbol in
      let i = skip_whitespace s i len in
      parse_args s i len (name :: acc)
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
  let loc = Loc (s, i) in
  if i < len
  then (
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
      let i, section, sections = parse_string_sections s (i + 1) len '"' in
      i, String (section, sections, loc)
    | '\'' ->
      let i, section, sections = parse_string_sections s (i + 1) len '\'' in
      (match sections with
       | [] ->
         (match String.length section with
          | 1 -> i, Char (section.[0], loc)
          | _ -> errorfn loc "Character literal must only describe single character.")
       | _ :: _ ->
         errorfn loc "Character literal must not contain any interpolated expressions.")
    | '(' ->
      let i, expr = parse_expr s (i + 1) len in
      let i = skip_exact_char s i len ')' in
      i, expr
    | '_' -> i + 1, Wildcard loc
    | _ ->
      errorfn
        loc
        "Expected to find the beginning of an expression, but found an unexpected \
         character '%c' instead."
        c)
  else errorfn loc "Expected to find an expression, but the program ended."

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
    | ')' | '=' | ';' | ':' | '|' | ',' | '}' -> i, List.rev acc
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

and parse_rest_of_sections s i len closer next acc =
  match next with
  | `Done -> i, List.rev acc
  | `Keep_going ->
    let i, expr = parse_expr s i len in
    let i = skip_exact_char s i len '}' in
    let i, section, next = parse_string_section s i len closer in
    parse_rest_of_sections s i len closer next ((expr, section) :: acc)

and parse_string_sections s i len closer =
  let i, section, next = parse_string_section s i len closer in
  let i, sections = parse_rest_of_sections s i len closer next [] in
  i, section, sections
;;

let rec parse_rest_of_definitions s i len acc =
  if i < len
  then (
    let i = skip_whitespace s i len in
    let i = skip_exact_char s i len ',' in
    let i = skip_whitespace s i len in
    let i, symbol = parse_symbol s i len in
    let name = expect_name_of_symbol s i symbol in
    let i = skip_whitespace s i len in
    let i = skip_exact_char s i len '=' in
    let i = skip_whitespace s i len in
    let i, expr = parse_expr s i len in
    parse_rest_of_definitions s i len ((name, expr) :: acc))
  else i, List.rev acc
;;

let parse_definitions s i len =
  if i < len
  then (
    let i, symbol = parse_symbol s i len in
    let name = expect_name_of_symbol s i symbol in
    let i = skip_whitespace s i len in
    let i = skip_exact_char s i len '=' in
    let i = skip_whitespace s i len in
    let i, expr = parse_expr s i len in
    let i = skip_whitespace s i len in
    parse_rest_of_definitions s i len [ name, expr ])
  else i, []
;;

let parse_program s =
  let len = String.length s in
  let i, expr = parse_definitions s 0 len in
  if i < len
  then
    errorfn
      (Loc (s, i))
      "Finished parsing program before the end of the input text was reached."
  else expr
;;

let parse_standalone_expr s =
  let len = String.length s in
  let i, expr = parse_expr s 0 len in
  if i < len
  then
    errorfn
      (Loc (s, i))
      "Finished parsing expression before the end of the input text was reached."
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

let format_char_contents buf c =
  match c with
  | '\t' -> Buffer.add_string buf "\\t"
  | '\n' -> Buffer.add_string buf "\\n"
  | '\\' -> Buffer.add_string buf "\\\\"
  | '{' -> Buffer.add_string buf "\\{"
  | '"' -> Buffer.add_string buf "\\\""
  | '\'' -> Buffer.add_string buf "\\\'"
  | c -> Buffer.add_char buf c
;;

let format_string_contents buf s = String.iter s ~f:(format_char_contents buf)

let rec format_expr buf indent parent expr =
  match expr with
  | Wildcard _ -> Buffer.add_string buf "_"
  | Name (name, _) -> Buffer.add_string buf name
  | Integer (i, _) -> Buffer.add_string buf (Int.to_string i)
  | String (section, sections, _) ->
    Buffer.add_char buf '"';
    format_string_contents buf section;
    List.iter sections ~f:(fun (expr, section) ->
      Buffer.add_char buf '{';
      format_expr buf indent `Non_match expr;
      Buffer.add_char buf '}';
      format_string_contents buf section);
    Buffer.add_char buf '"'
  | Char (c, _) ->
    Buffer.add_char buf '\'';
    format_char_contents buf c;
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
    Buffer.add_string buf " =";
    let expr_indent = space_or_newline_and_indent buf indent expr in
    format_expr buf expr_indent `Non_match expr;
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
    Buffer.add_string buf "match";
    let expr_indent = space_or_newline_and_indent buf indent expr in
    format_expr buf expr_indent `Match expr;
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

let format_definitions buf indent definitions =
  match definitions with
  | [] -> errorfn Noloc "BUG: no definitions found in program"
  | (name, expr) :: tl ->
    Buffer.add_string buf name;
    Buffer.add_string buf " =";
    let new_indent = space_or_newline_and_indent buf indent expr in
    format_expr buf new_indent `Non_match expr;
    List.iter tl ~f:(fun (name, expr) ->
      Buffer.add_string buf ",\n";
      format_indent buf indent;
      Buffer.add_string buf name;
      Buffer.add_string buf " =";
      let new_indent = space_or_newline_and_indent buf indent expr in
      format_expr buf new_indent `Non_match expr)
;;

type value =
  | Vdata of string
  | Vinteger of int
  | Vstring of string
  | Vchar of char
  | Vfun of expr list * expr
  | Vcall of value * value list
  | Vbuiltin_fun of (value list -> value)
  | Varray of value array

(* Abstract values like arrays do not have equivalent expressions, so this
   function cannot be trusted; it is mainly for debugging and printing purposes. *)
let rec value_to_expr value =
  match value with
  | Vdata name -> Data (name, Noloc)
  | Vinteger i -> Integer (i, Noloc)
  | Vstring s -> String (s, [], Noloc)
  | Vchar c -> Char (c, Noloc)
  | Vfun (args, body) -> Fun (args, body, Noloc)
  | Vcall (name, args) -> Call (value_to_expr name, List.map args ~f:value_to_expr, Noloc)
  | Vbuiltin_fun _ -> Data ("Abstract_builtin_fun", Noloc)
  | Varray _ -> Data ("Abstract_array", Noloc)
;;

let expect_arg args =
  match args with
  | [] -> abortfn Noloc "Not enough args."
  | arg :: args -> arg, args
;;

let expect_array args =
  let arg, args = expect_arg args in
  match arg with
  | Vdata _ | Vinteger _ | Vstring _ | Vchar _ | Vfun _ | Vcall _ | Vbuiltin_fun _ ->
    abortfn Noloc "Expected array value."
  | Varray array -> array, args
;;

let expect_int args =
  let arg, args = expect_arg args in
  match arg with
  | Vdata _ | Vstring _ | Vchar _ | Vfun _ | Vcall _ | Vbuiltin_fun _ | Varray _ ->
    abortfn Noloc "Expected int value."
  | Vinteger int -> int, args
;;

let expect_string args =
  let arg, args = expect_arg args in
  match arg with
  | Vdata _ | Vinteger _ | Vchar _ | Vfun _ | Vcall _ | Vbuiltin_fun _ | Varray _ ->
    abortfn Noloc "Expected array value."
  | Vstring value -> value, args
;;

let expect_no_more_args args =
  match args with
  | [] -> ()
  | _ :: _ -> abortfn Noloc "Too many args."
;;

let initial_context argv : value String_map.t =
  String_map.of_list
    [ ( "array_length"
      , Vbuiltin_fun
          (fun args ->
            let array, args = expect_array args in
            let () = expect_no_more_args args in
            Vinteger (Array.length array)) )
    ; ( "array_get"
      , Vbuiltin_fun
          (fun args ->
            let array, args = expect_array args in
            let index, args = expect_int args in
            let () = expect_no_more_args args in
            array.(index)) )
    ; "argv", Varray (Array.map argv ~f:(fun arg -> Vstring arg))
    ; ( "print"
      , Vbuiltin_fun
          (fun args ->
            let value, args = expect_string args in
            let () = expect_no_more_args args in
            Printf.printf "%s%!" value;
            Vdata "T") )
    ; ( "abort"
      , Vbuiltin_fun
          (fun args ->
            let value, args = expect_string args in
            let () = expect_no_more_args args in
            errorfn Noloc "%s" value) )
    ; ( "read_file"
      , Vbuiltin_fun
          (fun args ->
            let value, args = expect_string args in
            let () = expect_no_more_args args in
            Vstring (In_channel.with_open_bin value In_channel.input_all)) )
    ; ( "int_to_string"
      , Vbuiltin_fun
          (fun args ->
            let value, args = expect_int args in
            let () = expect_no_more_args args in
            Vstring (Int.to_string value)) )
    ; ( "int_compare"
      , Vbuiltin_fun
          (fun args ->
            let a, args = expect_int args in
            let b, args = expect_int args in
            let () = expect_no_more_args args in
            Vdata
              (if a < b then "Less_than" else if a > b then "Greater_than" else "Equal"))
      )
    ; ( "string_length"
      , Vbuiltin_fun
          (fun args ->
            let string, args = expect_string args in
            let () = expect_no_more_args args in
            Vinteger (String.length string)) )
    ; ( "string_get"
      , Vbuiltin_fun
          (fun args ->
            let string, args = expect_string args in
            let index, args = expect_int args in
            let () = expect_no_more_args args in
            Vchar string.[index]) )
    ]
;;

let value_to_string value =
  let buf = Buffer.create 1024 in
  format_expr buf 0 `Non_match (value_to_expr value);
  Buffer.contents buf
;;

let print_value value = printfn "%s" (value_to_string value)

let rec evaluate_expr context expr =
  match expr with
  | Wildcard loc -> abortfn loc "Wildcard expression used as value."
  | Name (name, loc) ->
    (match String_map.find_opt name context with
     | None -> abortfn loc "Name '%s' is not defined." name
     | Some value -> value)
  | Data (name, _) -> Vdata name
  | Integer (i, _) -> Vinteger i
  | String (section, sections, _) ->
    let buf = Buffer.create (String.length section) in
    Buffer.add_string buf section;
    List.iter sections ~f:(fun (expr, section) ->
      let value = evaluate_expr context expr in
      (match value with
       | Vstring s -> Buffer.add_string buf s
       | Vchar c -> Buffer.add_char buf c
       | Vdata _ | Vinteger _ | Vfun (_, _) | Vcall (_, _) | Vbuiltin_fun _ | Varray _ ->
         let loc = loc_of_expr expr in
         abortfn
           loc
           "Attempted to interpolate value that is neither a string nor a character.");
      Buffer.add_string buf section);
    Vstring (Buffer.contents buf)
  | Char (value, _) -> Vchar value
  | Fun (args, body, _) -> Vfun (args, body)
  | Let (pattern, expr, body, _) -> evaluate_match context expr [ [ pattern ], body ]
  | Seq (a, b, loc) -> evaluate_match context a [ [ Data ("T", loc) ], b ]
  | Match (expr, cases, _) -> evaluate_match context expr cases
  | Call (fun_, args, loc) ->
    let fun_ = evaluate_expr context fun_ in
    let args = List.map args ~f:(fun arg -> evaluate_expr context arg) in
    (match fun_ with
     | Vdata _ -> Vcall (fun_, args)
     | Vinteger _ -> abortfn loc "Attempted to call an integer value."
     | Vstring _ -> abortfn loc "Attempted to call a string value."
     | Vchar _ -> abortfn loc "Attempted to call a char value."
     | Vcall _ -> abortfn loc "Attempted to call a call value."
     | Vfun (arg_patterns, body) -> evaluate_call context arg_patterns args body
     | Vbuiltin_fun f -> f args
     | Varray _ -> abortfn loc "Attempted to call an array value.")

and evaluate_call context arg_patterns args body =
  match arg_patterns with
  | [] ->
    (match args with
     | [] -> evaluate_expr context body
     | _ :: _ -> abortfn Noloc "Too many arguments.")
  | arg_pattern :: arg_patterns ->
    (match args with
     | [] -> abortfn Noloc "Not enough args."
     | arg :: args ->
       (match evaluate_pattern context arg_pattern arg with
        | None -> abortfn Noloc "No patterns matched value."
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
  | None ->
    let value = value_to_string value in
    abortfn (loc_of_expr expr) "No patterns matched value:\n%s" value
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
     | Vdata vname -> if String.equal name vname then Some context else None
     | Vinteger _ | Vstring _ | Vchar _ | Vfun _ | Vcall _ | Vbuiltin_fun _ | Varray _ ->
       None)
  | Integer (i, _) ->
    (match value with
     | Vinteger vi -> if Int.equal i vi then Some context else None
     | Vdata _ | Vstring _ | Vchar _ | Vfun _ | Vcall _ | Vbuiltin_fun _ | Varray _ ->
       None)
  | String (string, sections, loc) ->
    (match sections with
     | [] ->
       (match value with
        | Vstring vstring -> if String.equal string vstring then Some context else None
        | Vdata _ | Vinteger _ | Vchar _ | Vfun _ | Vcall _ | Vbuiltin_fun _ | Varray _ ->
          None)
     | _ :: _ -> abortfn loc "Attempted to use interpolated string as pattern.")
  | Char (c, _) ->
    (match value with
     | Vchar vc -> if Char.equal c vc then Some context else None
     | Vdata _ | Vinteger _ | Vstring _ | Vfun _ | Vcall _ | Vbuiltin_fun _ | Varray _ ->
       None)
  | Fun (_, _, loc) -> abortfn loc "Attempted to use function expression as pattern."
  | Let (_, _, _, loc) -> abortfn loc "Attempted to use let expression as pattern."
  | Seq (_, _, loc) ->
    abortfn loc "Attempted to use a sequence of two expressions as pattern."
  | Call (fun_, args, _) ->
    (match value with
     | Vcall (vfun_, vargs) ->
       evaluate_call_patterns context (fun_ :: args) (vfun_ :: vargs)
     | Vdata _ | Vinteger _ | Vchar _ | Vstring _ | Vfun _ | Vbuiltin_fun _ | Varray _ ->
       None)
  | Match (_, _, loc) -> abortfn loc "Attempted to use match expression as pattern."
;;

let rec evaluate_program context definitions =
  match definitions with
  | [] -> context
  | (name, expr) :: tl ->
    let value = evaluate_expr context expr in
    let context =
      if String_map.mem name context
      then
        abortfn
          Noloc
          "Attempted to define top-level value '%s', but this name is already defined."
          name
      else String_map.add name value context
    in
    evaluate_program context tl
;;

let expr_value_names definitions = List.map definitions ~f:(fun (name, _) -> name)

let print_help () =
  printfn "Commands:";
  printfn "    format FILE           Print the formatted form of code in a file";
  printfn "    run FILE              Run a file";
  printfn "    repl                  Start an interactive interpreter session";
  printfn
    "    list-value-names      Print a list of all the top-level definitions in a file."
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
         format_definitions buf 0 parsed;
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
           let parsed = parse_standalone_expr line in
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
       | _ ->
         let filename = Sys.argv.(2) in
         let contents = In_channel.with_open_bin filename In_channel.input_all in
         let parsed = parse_program contents in
         let context = initial_context (Array.sub Sys.argv ~pos:2 ~len:(num_args - 2)) in
         let context = evaluate_program context parsed in
         let (_ : value) =
           evaluate_expr
             context
             (Let
                ( Data ("T", Noloc)
                , Call (Name ("main", Noloc), [ Data ("T", Noloc) ], Noloc)
                , Data ("T", Noloc)
                , Noloc ))
         in
         ())
    | "list-value-names" ->
      (match num_args with
       | 0 | 1 | 2 ->
         printfn "Not enough arguments";
         print_help ()
       | 3 ->
         let filename = Sys.argv.(2) in
         let contents = In_channel.with_open_bin filename In_channel.input_all in
         let parsed = parse_program contents in
         let value_names = expr_value_names parsed in
         List.iter value_names ~f:(printfn "%s")
       | _ ->
         printfn "Too many arguments";
         print_help ())
    | _ -> errorfn Noloc "'%s' is not a recognized command." command)
;;
