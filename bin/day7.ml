open Utils

module OperatorCheck = struct

  let create result int_list =
    (result, int_list)

  let check_possible oc partb =
    let (result, int_list) = oc in
    let rec generate_possibilities l running_results =
      match l with
      | [] -> running_results
      | [a] ->
        if running_results = [] then [a]
        else
        List.concat [List.map (fun x -> x + a) running_results;
                     List.map (fun x -> x * a) running_results;
                     if partb then (List.map (fun x -> (int_of_string ((string_of_int x) ^ (string_of_int a)))) running_results) else []]
      | a::b::rest ->
        if running_results = [] then
          generate_possibilities rest (List.concat [[a + b;a * b];
                                                    if partb then [(int_of_string ((string_of_int a) ^ (string_of_int b)))] else []])
        else
          let new_running_list =
            List.concat [List.map (fun x -> x + a) running_results;
                         List.map (fun x -> x * a) running_results;
                         if partb then (List.map (fun x -> (int_of_string ((string_of_int x) ^ (string_of_int a)))) running_results) else []]
          in
          generate_possibilities (b::rest) new_running_list
    in List.mem result (generate_possibilities int_list [])

  let get_result (r, _) = r
end

let () =
  let input_file = "inputs/day7.txt" in
  let parsed =
  input_file
  |> read_lines
  |> List.filter (fun x -> String.length x > 1)
  |> List.map (String.split_on_char ':')
  |> List.map (fun x -> List.map String.trim x)
  |> List.filter (fun x -> List.length x = 2)
  |> List.map (fun x -> ((x |> List.hd |> int_of_string, x |> List.tl |> List.hd |> String.split_on_char ' ' |> List.map int_of_string)))
  |> List.map (fun (x, y) -> OperatorCheck.create x y)
  in
  parsed
  |> List.filter_map (fun x -> if OperatorCheck.check_possible x false then (Some x) else None)
  |> List.map OperatorCheck.get_result
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int
  |> print_endline;
  parsed
  |> List.filter_map (fun x -> if OperatorCheck.check_possible x true then (Some x) else None)
  |> List.map OperatorCheck.get_result
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int
  |> print_endline;
