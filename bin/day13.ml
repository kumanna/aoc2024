open Utils

module Game = struct
  type t = {
      a_offset : int * int;
      b_offset : int * int;
      prize_x : int;
      prize_y : int;
    }

  let create s1 s2 s3 =
    let parse_offsets offset s =
      match
      (String.sub s offset ((String.length s) - offset)
      |> String.split_on_char ','
      |> List.map (String.trim)
      |> (List.map (fun x -> String.sub x 2 ((String.length x) - 2)))
      |> List.map int_of_string) with
      | [a;b] -> (a, b)
      | _ -> (-1, -1)
    in
    let a_offset = parse_offsets 10 s1 in
    let b_offset = parse_offsets 10 s2 in
    let (prize_x, prize_y) = parse_offsets 6 s3 in
    { a_offset ; b_offset ; prize_x ; prize_y }

  (* Ignored the case where *)
  (* the rows are scaled versions of each other *)
  let find_feasibility { a_offset ; b_offset ; prize_x ; prize_y } partb =
    let prize_x = if partb then 10000000000000 + prize_x else prize_x in
    let prize_y = if partb then 10000000000000 + prize_y else prize_y in
    let (a_offset_x, a_offset_y) = a_offset in
    let (b_offset_x, b_offset_y) = b_offset in
    let determinant = a_offset_x * b_offset_y - b_offset_x * a_offset_y in
    let inverse_top_1 = b_offset_y * prize_x - b_offset_x * prize_y in
    let inverse_top_2 = -a_offset_y * prize_x + a_offset_x * prize_y in
    if determinant <> 0 && ((inverse_top_1 mod determinant) = 0) && ((inverse_top_2 mod determinant) = 0) then
      Some (3 * (inverse_top_1 / determinant) + (inverse_top_2 / determinant))
    else None

end

let parse_games l =
  let rec parse_games_helper l running_list =
    match l with
    | a::b::c::rest -> parse_games_helper rest ((Game.create a b c)::running_list)
    | _::rest -> parse_games_helper rest running_list
    | [] -> running_list
  in
  parse_games_helper l []

let () =
  let input_file = "inputs/day13.txt" in
  input_file
  |> read_lines
  |> List.filter (fun x -> String.length x > 0)
  |> parse_games
  |> List.filter_map (fun x -> Game.find_feasibility x false)
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int
  |> print_endline;
  input_file
  |> read_lines
  |> List.filter (fun x -> String.length x > 0)
  |> parse_games
  |> List.filter_map (fun x -> Game.find_feasibility x true)
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int
  |> print_endline;
