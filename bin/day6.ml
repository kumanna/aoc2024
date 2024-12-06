open Utils

let explode s = List.init (String.length s) (String.get s)

type direction =
  | Up
  | Down
  | Left
  | Right

module GridPositions = struct
  type t = { row : int ; column : int ; direction : direction}

  let create (row, column, direction) = { row ; column ; direction }

  let row p1 = p1.row

  let col p1 = p1.column

  let dir p1 = p1.direction

  let compare p1 p2 =
    if p1.row = p2.row then
      p1.column - p2.column
    else
      p1.row - p2.row
end

module PositionSet = Set.Make(GridPositions)

let rec identify_positions grid current_pos n_covered =
  if ((List.find_all (fun x -> x = current_pos) n_covered) |> List.length) > 1 then
    (current_pos::n_covered, true)
  else
    let row = GridPositions.row current_pos in
    let col = GridPositions.col current_pos in
    let direction = GridPositions.dir current_pos in
    let n_rows = Array.length grid in
    let n_columns = Array.length grid.(0) in
    if row < 0 || row > n_rows - 1 || col < 0 || col > n_columns - 1 then
      (n_covered, false)
    else
      let new_p_set = current_pos::n_covered in
      match direction with
      | Up -> if row = 0 then (new_p_set, false)
              else if grid.(row - 1).(col) = '#' then
                identify_positions grid (GridPositions.create (row, col + 1, Right)) new_p_set
              else
                identify_positions grid (GridPositions.create (row - 1, col, Up)) new_p_set
      | Right -> if col = n_columns - 1 then (new_p_set, false)
                 else if grid.(row).(col + 1) = '#' then
                   identify_positions grid (GridPositions.create (row + 1, col, Down)) new_p_set
                 else
                   identify_positions grid (GridPositions.create (row, col + 1, Right)) new_p_set
      | Down -> if row = n_columns - 1 then (new_p_set, false)
                else if grid.(row + 1).(col) = '#' then
                  identify_positions grid (GridPositions.create (row, col - 1, Left)) new_p_set
                else
                  identify_positions grid (GridPositions.create (row + 1, col, Down)) new_p_set
      | Left -> if col = 0 then (new_p_set, false)
                else if grid.(row).(col - 1) = '#' then
                  identify_positions grid (GridPositions.create (row - 1, col, Up)) new_p_set
                else
                  identify_positions grid (GridPositions.create (row, col - 1, Left)) new_p_set

let () =
  let input_file = "inputs/day6.txt" in
  let grid =
    input_file
    |> read_lines
    |> List.filter (fun x -> String.length x > 1)
    |> List.map explode
    |> List.map Array.of_list
    |> Array.of_list
  in
  let (start_row, start_column) =
    Array.mapi (fun i y -> (i, Array.find_index (fun x -> x = '^') y)) grid
    |> Array.to_list
    |> List.filter_map (fun (x, y) ->
        match y with
        | Some y_ -> Some (x, y_)
        | None -> None)
    |> List.hd
  in
  let current_position = GridPositions.create (start_row, start_column, Up) in
  let (position_list, _) = identify_positions grid current_position [] in
  position_list
  |> PositionSet.of_list
  |> PositionSet.cardinal
  |> string_of_int
  |> print_endline;
  let n_occlusions = ref 0 in
  let positions_to_test =
    position_list |> PositionSet.of_list |> PositionSet.remove current_position |> PositionSet.to_list |> Array.of_list in
  for i = 0 to ((Array.length positions_to_test) - 1) do
    let (r, c) = GridPositions.(row positions_to_test.(i), col positions_to_test.(i)) in
    if (grid.(r).(c) = '.') then
      (print_endline ("(" ^ (string_of_int r) ^ ", " ^ (string_of_int c) ^ "): " ^ (string_of_int !n_occlusions));
       grid.(r).(c) <- '#';
       let (_, is_cycle) = identify_positions grid current_position [] in
       n_occlusions := !n_occlusions + (if is_cycle then 1 else 0);
       grid.(r).(c) <- '.';)
  done;
  (!n_occlusions) |> string_of_int |> print_endline
