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

let chomp_string s =
  let rec chomp_string_helper unchomped_string running_string add_now =
    let l = String.length unchomped_string in
    if l < 4 then
      if add_now then
        running_string ^ unchomped_string
      else
        running_string
    else if (String.sub unchomped_string 0 4) = "do()" then
      chomp_string_helper
        (String.sub unchomped_string 4 (l - 4))
        running_string
        true
    else if l > 7 && (String.sub unchomped_string 0 7) = "don't()" then
      chomp_string_helper
        (String.sub unchomped_string 7 (l - 7))
        running_string
        false
    else
      chomp_string_helper
        (String.sub unchomped_string 1 (l - 1))
        (running_string ^ (if add_now then (String.sub unchomped_string 0 1) else ""))
        add_now
  in
  chomp_string_helper s "" true

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
    |> print_endline;
    input
    |> read_lines
    |> String.concat ""
    |> chomp_string
    |> get_mul_strings
    |> string_of_int
    |> print_endline;
