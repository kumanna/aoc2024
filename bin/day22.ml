open Utils

let next_secret_number n =
  let s = (((n * 64) lxor n) mod 16777216) in
  let s = (((s / 32) lxor s) mod 16777216) in
  ((s * 2048) lxor s) mod 16777216;;

let find_2000th_number n =
  let s = ref n in
  for _ = 1 to 2000 do
    s := next_secret_number !s
  done;
  !s

let () =
  let input_file = "inputs/day22.txt" in
  input_file
  |> read_lines
  |> List.filter (fun x -> String.length x > 0)
  |> List.map int_of_string
  |> List.map find_2000th_number
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int
  |> print_endline
