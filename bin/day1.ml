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

let part1 sorted_values =
  let num1, num2 =
    Option.value_exn
      (two_sum_sorted sorted_values 2020 0 (Array.length sorted_values - 1))
  in
  printf "%d\n" (num1 * num2)

let rec three_sum_sorted nums target i =
  if i = Array.length nums then None
  else
    let num = nums.(i) in
    let excluded =
      Array.concat
        [
          Array.sub nums ~pos:0 ~len:i;
          Array.sub nums ~pos:(i + 1) ~len:(Array.length nums - i - 1);
        ]
    in
    let two_sum_result =
      two_sum_sorted excluded (target - num) 0 (Array.length excluded - 1)
    in
    match two_sum_result with
    | None -> three_sum_sorted nums target (i + 1)
    | Some (num2, num3) -> Some (num, num2, num3)

let part2 sorted_values =
  let num1, num2, num3 =
    Option.value_exn (three_sum_sorted sorted_values 2020 0)
  in
  printf "%d\n" (num1 * num2 * num3)

let () =
  let lines = In_channel.read_lines "./data/day1.txt" in
  let values = Array.of_list_map lines ~f:Int.of_string in
  Array.sort values ~compare:Int.compare;
  part1 values;
  part2 values
