open Utils

let rec successive_diff = function
  | [] -> []
  | [_] -> []
  | a::b::rest -> (b - a)::(successive_diff (b::rest))

let rec check_for_same_signs current_sign = function
  | [] -> true
  | [a] -> if (abs a) > 3 || (abs a) < 1 then false else a * current_sign > 0
  | a::b::rest -> if a * b < 0 then
                    false else if (abs a) > 3 || (abs a) < 1 then false
                  else check_for_same_signs (a / (abs a)) (b::rest)

let () =
  let input = "inputs/day2.txt" in
  input
  |> read_lines
  |> List.map (String.split_on_char ' ')
  |> List.map (fun x -> List.filter (fun y -> String.length y > 0) x)
  |> List.filter (fun x -> List.length x > 1)
  |> List.map (fun x -> List.map int_of_string x)
  |> List.map successive_diff
  |> List.map (check_for_same_signs 1)
  |> List.filter (fun x -> x)
  |> List.length
  |> string_of_int
  |> print_endline
