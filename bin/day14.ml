open Utils

let n_rows = 103

let n_cols = 101

module Robot = struct

  type t = {
    position_row : int ;
    position_col : int ;
    velocity_row : int ;
    velocity_col : int ;
  }

  let create s =
    let split_string = s |> String.split_on_char ' ' |> List.map (fun x -> String.sub x 2 ((String.length x) - 2)) |> List.map (fun x -> String.split_on_char ',' x) in
    match split_string with
    | [[position_col_;position_row_];
       [velocity_col_;velocity_row_]] ->
        let position_row = int_of_string position_row_ in
        let position_col = int_of_string position_col_ in
        let velocity_row = int_of_string velocity_row_ in
        let velocity_col = int_of_string velocity_col_ in
      {
        position_row ;
        position_col ;
        velocity_row ;
        velocity_col ;
      }
    | _ -> {
        position_row = -1 ;
        position_col = -1 ;
        velocity_row = -1 ;
        velocity_col = -1 ;
      }

  let execute_move n {
        position_row ;
        position_col ;
        velocity_row ;
        velocity_col ;
      } =
    let new_position_row = (position_row + velocity_row * n) mod n_rows in
    let new_position_col = (position_col + velocity_col * n) mod n_cols in
    {
      position_row = if new_position_row < 0 then new_position_row + n_rows else new_position_row ;
      position_col = if new_position_col < 0 then new_position_col + n_cols else new_position_col ;
      velocity_row = velocity_row ;
      velocity_col = velocity_col ;
    }

  let get_position r =
    (r.position_row, r.position_col)

end

let () =
  let input_file = "inputs/day14.txt" in
  let final_positions =
  input_file
  |> read_lines
  |> List.filter (fun x -> String.length x > 0)
  |> List.map Robot.create
  |> List.map (Robot.execute_move 100)
  |> List.map (Robot.get_position)
  in
  let row_boundary = n_rows / 2 in
  let col_boundary = n_cols / 2 in
  let q1 =
    List.filter (fun (row, col) -> row < row_boundary && col < col_boundary) final_positions
    |> List.length in
  let q2 =
    List.filter (fun (row, col) -> row > row_boundary && col < col_boundary) final_positions
    |> List.length in
  let q3 =
    List.filter (fun (row, col) -> row > row_boundary && col > col_boundary) final_positions
    |> List.length in
  let q4 =
    List.filter (fun (row, col) -> row < row_boundary && col > col_boundary) final_positions
    |> List.length in
  q1 * q2 * q3 * q4 |> string_of_int |> print_endline;
  let robots =
    input_file
    |> read_lines
    |> List.filter (fun x -> String.length x > 0)
    |> List.map Robot.create
  in
  let current_robots = ref robots in
  for i = 1 to (n_rows * n_cols - 1) do
    current_robots := List.map (Robot.execute_move 1) (!current_robots);
    let unique_positions =
      !current_robots
      |> List.map Robot.get_position
      |> List.sort_uniq compare
      |> List.length in
    if (List.length robots) = unique_positions then
      print_endline (string_of_int i);
  done
