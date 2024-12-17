(* INCOMPLETE *)

open Utils

type direction =
  | Up
  | Down
  | Left
  | Right

let find_char_index grid character =
    Array.mapi (fun i y -> (i, Array.find_index (fun x -> x = character) y)) grid
    |> Array.to_list
    |> List.filter_map (fun (x, y) ->
        match y with
        | Some y_ -> Some (x, y_)
        | None -> None)
    |> List.hd

let find_neighbours grid (row, col, _) =
  [(row + 1, col, Down); (row - 1, col, Up); (row, col + 1, Right); (row, col - 1, Left)]
  |> List.filter (fun (r, c, _) ->
      r >= 0 && r < (Array.length grid) &&
      c >= 0 && c < (Array.length grid.(0)) &&
      (grid.(r).(c) = '.' || grid.(r).(c) = 'E'))

let find_paths grid (start_row, start_col) end_point =
  let rec find_paths_helper running_paths =
    if List.length running_paths = 0 then
      failwith "Empty path list!";
    let augment_single_path single_path =
      match single_path with
      | [] -> failwith "No starting point!"
      | a::rest ->
        let (r_, c_, _) = a in
        if (r_, c_) = end_point then
          [single_path]
        else
          find_neighbours grid a
          |> List.filter (fun (r, c, _) -> not (List.mem (r, c) (rest |> List.map (fun (r_, c_, _) -> (r_, c_)))))
          |> List.map (fun x -> x::a::rest)
    in
    if (running_paths
       |> List.map List.hd
       |> List.map (fun (r, c, _) -> (r, c))
       |> List.fold_left (fun x y -> x && y = end_point) true) then
      running_paths
    else
      let new_paths =
        running_paths |> List.map augment_single_path |> List.concat
      in find_paths_helper new_paths
  in
  find_paths_helper [[(start_row, start_col, Right)]] |> (List.map List.rev)

let compute_score l =
  let rec compute_score_helper running_score l previous_direction =
    match l with
    | [] -> running_score
    | a::rest ->
      let (_, _, dir) = a in
      if previous_direction = dir then compute_score_helper (1 + running_score) rest dir
      else compute_score_helper (1001 + running_score) rest dir
  in
  let (hd, tl) = (List.hd l, List.tl l) in
  let (_, _, idir) = hd in
  compute_score_helper 0 tl idir

let () =
  let input_file = "inputs/day16.txt" in
  let grid =
    input_file
    |> read_lines
    |> List.filter (fun x -> String.length x > 0)
    |> List.map explode
    |> List.map Array.of_list
    |> Array.of_list
  in
  let (start_row, start_column) =
    find_char_index grid 'S' in
  let (end_row, end_column) =
    find_char_index grid 'E' in
  find_paths grid (start_row, start_column) (end_row, end_column)
  |> List.map compute_score
  |> List.fold_left (fun x y -> min x y) 10000000000
  |> string_of_int
  |> print_endline

