open Utils

let get_product s =
  s
  |> (fun x -> String.sub x 4 ((String.length x) - 5))
  |> String.split_on_char ','
  |> (fun x ->
      match x with
      | [a;b] ->
        (match (int_of_string_opt a, int_of_string_opt b) with
        | (Some a, Some b) ->
          if a < 1000 && b < 1000 && a > 0 && b > 0
          then a * b else 0
        | _ -> 0)
      | _ -> 0)

let get_mul_strings s =
  let mul_regexp = Str.regexp "mul[(][0-9][0-9]*,[0-9][0-9]*[)]" in
  let rec get_mul_string_helper n running_sum =
    let new_index = try Str.search_forward mul_regexp s n
      with Not_found -> -1
    in
    if new_index >= 0 then
      let matched_str = Str.matched_string s in
      get_mul_string_helper (new_index + (String.length matched_str)) ((get_product matched_str) + running_sum)
    else
      running_sum
  in
  get_mul_string_helper 0 0

let () =
    let input = "inputs/day3.txt" in
    input
    |> read_lines
    |> List.map get_mul_strings
    |> List.fold_left (fun x y -> x + y) 0
    |> string_of_int
    |> print_endline
