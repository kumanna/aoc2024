open Utils

let parse_rules r =
  match (String.split_on_char '|' r) with
  | [a;b] -> Some (int_of_string a, int_of_string b)
  | _ -> None

let parse_lists s =
  s
  |> String.split_on_char ','
  |> List.map int_of_string

let check_list_validity l rules =
  let rec check_list_validity_helper = function
    | [] -> false
    | [_] -> true
    | a::rest ->
      let valid =
        rest
        |> List.map (fun x -> (a, x))
        |> List.map (fun x -> List.mem x rules)
        |> List.fold_left (fun x y -> x && y) true
      in
      if valid then check_list_validity_helper rest
      else false
  in
  check_list_validity_helper l

let () =
  let input_file = "inputs/day5.txt" in
  let (rules, int_lists) =
  input_file
  |> read_lines
  |> List.filter (fun x -> String.length x > 1)
  |> (fun x -> (List.filter (fun a -> String.contains a '|') x,
                List.filter (fun a -> String.contains a ',') x))
  |> (fun (x, y) -> List.filter_map parse_rules x,
                    List.map parse_lists y)
  in
  int_lists
  |> List.filter_map (fun x ->
      if (check_list_validity x rules)
      then Some x
      else None)
  |> List.map Array.of_list
  |> List.map (fun x -> x.((Array.length x) / 2))
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int
  |> print_endline
