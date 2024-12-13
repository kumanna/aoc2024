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

  let find_feasibility { a_offset ; b_offset ; prize_x ; prize_y } =
    let (a_offset_x, a_offset_y) = a_offset in
    let (b_offset_x, b_offset_y) = b_offset in
    let max_b_presses = min (prize_x / b_offset_x) (prize_y / b_offset_y) in
    let b_press_counts = List.init (max_b_presses + 1) (fun x -> x)
    in
    b_press_counts
    |> List.filter_map
         (fun n_b_presses ->
           let current_total_x = n_b_presses * b_offset_x in
           let current_total_y = n_b_presses * b_offset_y in
           let n_a_presses_1 = (prize_x - current_total_x) / a_offset_x in
           let n_a_presses_2 = (prize_y - current_total_y) / a_offset_y in
           if n_a_presses_1 = n_a_presses_2 &&
                n_a_presses_1 * a_offset_x + current_total_x = prize_x &&
                  n_a_presses_1 * a_offset_y + current_total_y = prize_y
           then
             Some (n_a_presses_1, n_b_presses)
           else
             None)
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
  |> List.map Game.find_feasibility
  |> List.filter (fun l -> List.length l > 0)
  |> List.map (fun l -> List.fold_left (fun x (a, b) -> min x (3 * a + b)) 1000000000 l)
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int
  |> print_endline
