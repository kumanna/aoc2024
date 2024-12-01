open Utils

let split_list_pairs l =
  let rec split_list_pair_helper running_list list1 list2 =
    match running_list with
    | [] -> (list1, list2)
    | [x;y]::rest -> split_list_pair_helper rest ((int_of_string x)::list1) ((int_of_string y)::list2)
    | _ -> failwith "Illegal list size!"
  in
  split_list_pair_helper l [] []

let count_occurrences element l =
  let rec count_occurrences_helper n = function
    | [] -> n
    | a::rest ->
      let next_n = if element = a then n + 1 else n in
        count_occurrences_helper next_n rest
  in
  count_occurrences_helper 0 l

let () =
  let split_lists =
    let input = "inputs/day1.txt" in
    input
    |> read_lines
    |> List.filter (fun x -> String.length x > 1)
    |> List.map (String.split_on_char ' ')
    |> List.map (List.filter (fun x -> String.length x > 0))
    |> split_list_pairs
  in
  split_lists
  |> (fun (x, y) -> (List.sort compare x, List.sort compare y))
  |> (fun (x, y) -> List.map2 (fun a b -> abs (a - b)) x y)
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int
  |> print_endline;
  (* For each element in the first list, we count the number of
     occurrences in the second list *)
  let (list1, list2) = split_lists in
  list1
  |> List.map (fun x -> x * (count_occurrences x list2))
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int
  |> print_endline;
