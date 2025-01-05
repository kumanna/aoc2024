open Utils

let final_values = Hashtbl.create 16

let () =
  let input_file = "inputs/day24.txt" in
  input_file
  |> read_lines
  |> List.filter (fun x -> String.contains x ':')
  |> List.map (String.split_on_char ':')
  |> List.iter (fun x ->
      match x with
      | [a;b] ->
        let bool_val = String.trim b = "1" in
        Hashtbl.add final_values a bool_val
      | _ -> ()
    );
  let original_relations =
    input_file
    |> read_lines
    |> List.filter (fun x -> String.contains x '-')
    |> List.map (String.split_on_char ' ')
    |> List.filter_map (fun x ->
        match x with
        | [l;op;r;_;res] ->
          Some (l, op, r, res)
        | _ -> None) |> Array.of_list in
  let op =
    [("AND", fun x y -> x && y);
     ("XOR", fun x y -> (x && (not y)) || (y && (not x)));
     ("OR", fun x y -> x || y)] in
  let relations = ref original_relations in
  while Array.length (!relations) > 0 do
    let new_relations = ref (Dynarray.create ()) in
    for i = 0 to ((Array.length !relations) - 1) do
      let (left, opname, right, result) = (!relations).(i) in
      if Hashtbl.mem final_values left && Hashtbl.mem final_values right then
        Hashtbl.add final_values result
          ((List.assoc opname op) (Hashtbl.find final_values left) (Hashtbl.find final_values right))
      else
        Dynarray.add_last !new_relations (!relations).(i)
    done;
    relations := !new_relations |> Dynarray.to_array
  done;
  let z_list = ref [] in
  Hashtbl.iter
    (fun a b ->
       if String.get a 0 = 'z' then
         z_list := (a, b)::(!z_list);
    ) final_values;
  !z_list
  |> List.sort compare
  |> List.iter (fun (x, y) ->
      print_endline (x ^ ": " ^ (string_of_bool y)));
  let s = ref "" in
  !z_list
  |> List.sort compare
  |> List.iter (fun (_, y) ->
      s := (if y then "1" else "0") ^ !s);
  print_int (int_of_string ("0b" ^ !s));
  print_endline "";
