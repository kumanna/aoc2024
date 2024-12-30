open Utils

let pair_dict = Hashtbl.create 16

module Triplet = struct

  let create a b c =
    match List.sort_uniq compare [a;b;c] with
    | [x;y;z] -> Some (x, y, z)
    | _ -> None

end

let () =
  let input_file = "inputs/day23.txt" in
  let pairs =
    input_file
    |> read_lines
    |> List.filter (fun x -> String.length x > 0)
    |> List.map (String.split_on_char '-')
    |> List.filter_map (fun x ->
        match x with
        | [a;b] -> Some (a, b)
        | _ -> None) in
  pairs
  |> (fun p -> [p;List.map (fun (x, y) -> (y, x)) p])
  |> List.concat
  |> List.iter (fun (x, y) ->
      match Hashtbl.find_opt pair_dict x with
      | Some l -> Hashtbl.replace pair_dict x (y::l)
      | None -> Hashtbl.add pair_dict x [y]);
  pairs
  |> List.map (fun (x, y) -> (x, y, (Hashtbl.find pair_dict y) |> List.filter (fun z -> z |> Hashtbl.find pair_dict |> List.mem x)))
  |> List.map (fun (x, y, z) -> List.map (fun u -> (x, y, u)) z)
  |> List.concat
  |> List.filter_map (fun (x, y, z) -> Triplet.create x y z)
  |> List.sort_uniq compare
  |> List.filter (fun (x, y, z) -> String.get x 0 = 't' || String.get y 0 = 't' || String.get z 0 = 't')
  |> List.length
  |> string_of_int
  |> print_endline
