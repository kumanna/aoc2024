open Utils

let transpose a_ =
  let a = Array.of_list (String.split_on_char '\n' a_) in
  let l = String.length a.(0) in
  let at = Array.make l "" in
  for i = 0 to (l - 1) do
    for j = 0 to (Array.length a) - 1 do
      at.(i) <- at.(i) ^ (Char.escaped (String.get a.(j) i))
    done
  done;
  at

let check_match l1 l2 =
  List.map2 (fun x y -> x + y) l1 l2 |> List.fold_left (fun x y -> x && (y <= 5)) true

let parse_lines (l: string list) =
  let rec parse_lines_helper (l: string list) running_lists =
    match l with
    | [] -> running_lists
    | a::rest ->
      match running_lists with
      | [] -> parse_lines_helper rest [a]
      | a_::rest_ ->
        if a = "" then
          parse_lines_helper rest (""::a_::rest_)
        else
          parse_lines_helper rest ((String.trim (a_ ^ "\n" ^ a))::rest_)
  in
  parse_lines_helper l [] |> List.rev

let () =
  let input_file = "inputs/day25.txt" in
  let locks =
  input_file
  |> read_lines
  |> parse_lines
  |> List.filter (fun s -> String.length s > 0)
  |> List.filter (fun s -> String.get s 0 = '#')
  |> List.map transpose
  |> List.map Array.to_list
  |> List.map (fun l -> List.map (fun s -> String.sub s 1 6) l)
  |> List.map (fun l -> l |> List.map (fun x -> x |> explode |> List.filter (fun s -> s = '#')
                                                |> List.length)) in
  let keys =
    input_file
    |> read_lines
    |> parse_lines
    |> List.filter (fun s -> String.length s > 0)
    |> List.filter (fun s -> String.get s 0 = '.')
    |> List.map transpose
    |> List.map Array.to_list
    |> List.map (fun l -> List.map (fun s -> String.sub s 0 6) l)
    |> List.map (fun l -> l |> List.map (fun x -> x |> explode |> List.filter (fun s -> s = '#')
                                                  |> List.length)) in
  let matches = ref 0 in
  let keys_ = Array.of_list keys in
  let locks_ = Array.of_list locks in
  for i = 0 to (Array.length keys_ - 1) do
    for j = 0 to (Array.length locks_ - 1) do
      if check_match keys_.(i) locks_.(j) then
        matches := !matches + 1
    done
  done;
  print_endline (string_of_int !matches)

