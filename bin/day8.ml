open Utils

let explode s = List.init (String.length s) (String.get s)

module Char_Set = Set.Make(Char)

let find_antinode (a1row, a1col) (a2row, a2col) grid =
  let rowdiff = a2row - a1row in
  let coldiff = a2col - a1col in
  let new_row = a1row - rowdiff in
  let new_col = a1col - coldiff in
  if new_row < 0 || new_col < 0 || (new_row > (Array.length grid) - 1) || (new_col > (Array.length grid.(0)) - 1) || (rowdiff = 0 && coldiff = 0) then
    None
  else
    Some (new_row, new_col)

let rec find_antinodes_partb (a1row, a1col) (a2row, a2col) running_list grid =
  let running_list = if running_list = [] then [(a1row, a1col);(a2row, a2col)] else running_list in
  let new_node = find_antinode (a1row, a1col) (a2row, a2col) grid in
  match new_node with
  | Some x -> find_antinodes_partb x (a1row, a1col) (x::running_list) grid
  | None -> running_list

let () =
  let input_file = "inputs/day8.txt" in
  let grid =
    input_file
    |> read_lines
    |> List.filter (fun x -> String.length x > 1)
    |> List.map explode
    |> List.map Array.of_list
    |> Array.of_list
  in
  let antenna_table = Hashtbl.create 16 in
  let cs = ref (Char_Set.empty) in
  for row = 0 to ((Array.length grid) - 1) do
    for col = 0 to ((Array.length grid.(0)) - 1) do
      if grid.(row).(col) != '.' then
        (Hashtbl.add antenna_table grid.(row).(col) (row, col);
         cs := Char_Set.add grid.(row).(col) !cs
        )
    done
  done;
  Char_Set.to_list !cs
  |> List.map (Hashtbl.find_all antenna_table)
  |> List.map (fun x -> List.map (fun y -> List.filter_map (fun z -> find_antinode z y grid) x) x)
  |> List.concat
  |> List.concat
  |> List.sort_uniq compare
  |> List.length
  |> string_of_int
  |> print_endline;
  Char_Set.to_list !cs
  |> List.map (Hashtbl.find_all antenna_table)
  |> List.map (fun x -> List.map (fun y -> List.map (fun z -> find_antinodes_partb z y [] grid) x) x) |> List.concat
  |> List.concat
  |> List.concat
  |> List.sort_uniq compare
  |> List.length
  |> string_of_int
  |> print_endline;