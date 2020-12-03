open Base
open Stdio

let rec two_sum_sorted nums target l r =
  if l < r then
    let l_val, r_val = (nums.(l), nums.(r)) in
    let sum = l_val + r_val in
    if sum = target then Some (l_val, r_val)
    else if sum > target then two_sum_sorted nums target l (r - 1)
    else two_sum_sorted nums target (l + 1) r
  else None

let () =
  let lines = In_channel.read_lines "./data/day1.txt" in
  let values = Array.of_list_map lines ~f:Int.of_string in
  let num1, num2 =
    Array.sort values ~compare:Int.compare;
    Option.value_exn (two_sum_sorted values 2020 0 (Array.length values - 1))
  in
  print_endline (Int.to_string (num1 * num2))
