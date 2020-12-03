open Base
open Stdio

let print_int_list lst =
  print_endline (String.concat (List.map lst ~f:Int.to_string) ~sep:",")
