open Utils

let explode s = List.init (String.length s) (String.get s)

module GridPositions = struct
  type t = { row : int ; column : int }

  let create (row, column) = { row ; column }

  let row p1 = p1.row

  let col p1 = p1.column

  let compare p1 p2 =
    if p1.row = p2.row then
      p1.column - p2.column
    else
      p1.row - p2.row
end

module PositionSet = Set.Make(GridPositions)

type direction =
  | Up
  | Down
  | Left
  | Right

let rec identify_positions grid current_pos n_covered direction =
  let row = GridPositions.row current_pos in
  let col = GridPositions.col current_pos in
  let n_rows = Array.length grid in
  let n_columns = Array.length grid.(0) in
  if row < 0 || row > n_rows - 1 || col < 0 || col > n_columns - 1 then
    n_covered
  else
    let new_p_set = PositionSet.add current_pos n_covered in
    match direction with
    | Up -> if row = 0 then new_p_set
      else if grid.(row - 1).(col) = '#' then
        identify_positions grid (GridPositions.create (row, col + 1)) new_p_set Right
      else
        identify_positions grid (GridPositions.create (row - 1, col)) new_p_set Up
  | Right -> if col = n_columns - 1 then new_p_set
    else if grid.(row).(col + 1) = '#' then
      identify_positions grid (GridPositions.create (row + 1, col)) new_p_set Down
    else
      identify_positions grid (GridPositions.create (row, col + 1)) new_p_set Right
  | Down -> if row = n_columns - 1 then new_p_set
      else if grid.(row + 1).(col) = '#' then
        identify_positions grid (GridPositions.create (row, col - 1)) new_p_set Left
      else
        identify_positions grid (GridPositions.create (row + 1, col)) new_p_set Down
  | Left -> if col = 0 then new_p_set
    else if grid.(row).(col - 1) = '#' then
      identify_positions grid (GridPositions.create (row - 1, col)) new_p_set Up
    else
      identify_positions grid (GridPositions.create (row, col - 1)) new_p_set Left

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
  let current_position = GridPositions.create (start_row, start_column) in
  identify_positions grid current_position PositionSet.empty Up
  |> PositionSet.cardinal
  |> string_of_int
  |> print_endline
