open Utils

let find_next_valid_points current_point grid =
  let max_rows = Array.length grid in
  let max_cols = Array.length grid.(0) in
  let (row, col) = current_point in
  let current_height = grid.(row).(col) in
  [if row > 0 && grid.(row - 1).(col) = current_height + 1 then
     Some (row - 1, col) else None;
   if row < (max_rows - 1) && grid.(row + 1).(col) = current_height + 1 then
     Some (row + 1, col) else None;
   if col > 0 && grid.(row).(col - 1) = current_height + 1 then
     Some (row, col - 1) else None;
   if col < (max_cols - 1) && grid.(row).(col + 1) = current_height + 1 then
     Some (row, col + 1) else None;
  ] |> List.filter_map (fun x -> x)

let find_paths grid =
  let start_points = ref [] in
  for i = 0 to ((Array.length grid) - 1) do
    for j = 0 to ((Array.length grid.(0)) - 1) do
      if grid.(i).(j) = 0 then start_points := (i, j)::!start_points
    done
  done;
  let rec find_paths_helper start_point_list =
    let (r, c) = start_point_list |> List.hd |> List.hd in
    let current_height = grid.(r).(c) in
    if current_height = 9 then start_point_list
    else
      List.map (fun x -> (x, find_next_valid_points (List.hd x) grid)) start_point_list |> List.map (fun (x, y) -> List.map (fun z -> z::x) y) |> List.concat |> find_paths_helper
  in
  find_paths_helper (List.map (fun x -> [x]) !start_points)

let () =
  let input_file = "inputs/day10.txt" in
  let grid =
    input_file
    |> read_lines
    |> List.filter (fun x -> String.length x > 1)
    |> List.map explode
    |> List.map (fun x -> List.map (fun y -> (int_of_char y) - (int_of_char '0')) x)
    |> List.map Array.of_list
    |> Array.of_list
  in
  find_paths grid
  |> List.map (fun x -> (List.hd x, x |> List.rev |> List.hd))
  |> List.sort_uniq compare
  |> List.length
  |> string_of_int
  |> print_endline;
  find_paths grid
  |> List.sort_uniq compare
  |> List.length
  |> string_of_int
  |> print_endline
