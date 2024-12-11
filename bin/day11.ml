open Utils

type split_value =
  | Single of int
  | Pair of int * int

let split_number n =
  if n = 0 then Single 1 else
    let ns = string_of_int n in
    let ns_length = (String.length ns) in
    if (ns_length mod 2) = 0 then
      Pair ((int_of_string (String.sub ns 0 (ns_length / 2))), (int_of_string (String.sub ns (ns_length / 2) (ns_length / 2))))
    else
      Single (n * 2024)

let count_table = Hashtbl.create 16

let rec process_stone n blinks =
  if blinks = 0 then
    (Hashtbl.add count_table (n, blinks) 1;
     1)
  else
    (match Hashtbl.find_opt count_table (n, blinks) with
    | Some result -> result
    | None ->
      let blink_result = split_number n in
      match blink_result with
      | Pair (x, y) ->
        (let result = (process_stone x (blinks - 1)) + (process_stone y (blinks - 1)) in
         Hashtbl.add count_table (n, blinks) result;
         result)
      | Single x ->
        (let result = process_stone x (blinks - 1) in
         Hashtbl.add count_table (n, blinks) result;
         result))

let () =
  let input_file = "inputs/day11.txt" in
  let initial_arrangement =
    input_file
    |> read_lines
    |> List.hd
    |> String.split_on_char ' '
    |> List.map int_of_string
  in
  initial_arrangement
  |> List.map (fun x -> process_stone x 25)
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int
  |> print_endline;
  initial_arrangement
  |> List.map (fun x -> process_stone x 75)
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int
  |> print_endline
