(*
   Rules:
     - Don't plan ahead; just go!
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
