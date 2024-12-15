open Utils

type direction =
  | Up
  | Down
  | Left
  | Right

let get_move_list grid move_function start_point =
  let rec get_move_list_helper start_point l =
    let (next_row, next_col) = move_function start_point in
    match grid.(next_row).(next_col) with
    | '.' -> Some l
    | 'O' -> (get_move_list_helper (next_row, next_col) ((next_row, next_col)::l))
    | '#' -> None
    | _ -> failwith "Invalid grid!"
  in
  get_move_list_helper start_point [start_point]

let execute_move grid start_point direction =
  let move_function (row, col) =
    match direction with
    | Up -> (row - 1, col)
    | Down -> (row + 1, col)
    | Left -> (row, col - 1)
    | Right -> (row, col + 1)
  in
  let moves_to_perform = get_move_list grid move_function start_point in
  match moves_to_perform with
  | Some moves ->
    (List.iter (fun (row, col) ->
        let (new_row, new_col) = move_function (row, col) in
        grid.(new_row).(new_col) <- grid.(row).(col)) moves;
     let (row, col) = start_point in
     grid.(row).(col) <- '.';
     move_function start_point)
  | None -> start_point

let () =
  let input_file = "inputs/day15.txt" in
  let lines =
    input_file
    |> read_lines
    |> Array.of_list
  in
  let parsed_data = ref [] in
  let running_string = ref "" in
  for i = 0 to (Array.length lines) - 1 do
    if (String.length lines.(i)) > 0 then
      running_string := String.trim ((!running_string) ^ "\n" ^ lines.(i))
    else
      (parsed_data := (!running_string)::(!parsed_data);
       running_string := ""
      )
  done;
  let parsed_data = List.rev !parsed_data in
  let (grid_str, move_list) =
    (parsed_data |> List.hd , parsed_data |> List.tl |> String.concat "" |> Str.global_replace (Str.regexp "\n") "" |> explode |> Array.of_list)
  in
  let grid =
    grid_str
    |> String.split_on_char '\n'
    |> List.map explode
    |> List.map Array.of_list
    |> Array.of_list
  in
  let (start_row, start_column) =
    Array.mapi (fun i y -> (i, Array.find_index (fun x -> x = '@') y)) grid
    |> Array.to_list
    |> List.filter_map (fun (x, y) ->
        match y with
        | Some y_ -> Some (x, y_)
        | None -> None)
    |> List.hd
  in
  let current_point = ref (start_row, start_column) in
  for i = 0 to ((Array.length move_list) - 1) do
    let d =
      match move_list.(i) with
      | '<' -> Left
      | '>' -> Right
      | '^' -> Up
      | 'v' -> Down
      | _ -> failwith "Illegal direction"
    in
    current_point := execute_move grid (!current_point) d
  done;
  let score = ref 0 in
  for row = 1 to ((Array.length grid) - 1) do
    for col = 1 to ((Array.length grid.(0)) - 1) do
      if grid.(row).(col) = 'O' then
        score := !score + 100 * row + col
    done
  done;
  !score |> string_of_int |> print_endline
