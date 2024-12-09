open Utils

let explode s = List.init (String.length s) (String.get s)

type memory_contents =
  | Blank of int
  | File of int * int

let rec process_numbers l running_list file_id blank =
  match l with
  | [] -> running_list |> List.rev
  | a::rest -> if blank then
                 process_numbers rest ((Blank a)::running_list) file_id false
               else
                 process_numbers rest ((File (file_id, a))::running_list) (file_id + 1) true

let check_array_sorted a =
  let valid = ref true in
  let files_done = ref false in
  for i = 0 to ((Array.length a) - 1) do
    match Array.get a i with
    | File (_, _) -> if !files_done then valid := false
    | Blank _ -> if not !files_done then files_done := true;
  done;
  !valid

let fill_blank_element a =
  let al = Array.length a in
  if check_array_sorted a then a
  else
    let da = Dynarray.of_list [] in
    let filled = ref false in
    for i = 0 to (al - 1) do
      match a.(i) with
      | File (id, filesize) -> Dynarray.add_last da (File (id, filesize))
      | Blank blanksize ->
         if !filled then Dynarray.add_last da (Blank blanksize) else
           match a.(al - 1) with
           | Blank _ -> failwith "Blank ending! :-("
           | File (id, filesize) ->
              if filesize > blanksize then
                (Dynarray.add_last da (File (id, blanksize));
                 a.(al - 1) <- File (id, filesize - blanksize);
                 filled := true)
              else if filesize = blanksize then
                (Dynarray.add_last da (File (id, blanksize));
                 a.(al - 1) <- Blank 0;
                 filled := true)
              else
                (Dynarray.add_last da (File (id, filesize));
                 Dynarray.add_last da (Blank (blanksize - filesize));
                 a.(al - 1) <- Blank 0;
                 filled := true);
    done;
    let blanks_removed = ref false in
    while not !blanks_removed do
      match Dynarray.get da ((Dynarray.length da) - 1) with
      | Blank _ -> Dynarray.remove_last da
      | File _ -> blanks_removed := true
    done;
    Dynarray.to_array da

let consolidate_blanks a =
  Array.fold_left (fun x y ->
      let ax = (Array.length x) in
      match (x.(ax - 1), y) with
      | (Blank a, Blank b) -> Array.concat [Array.sub x 0 (ax - 1);[|Blank (a + b)|]]
      | _ -> Array.concat [x;[|y|]]) (Array.of_list [a.(0)]) (Array.sub a 1 ((Array.length a) - 1))

let find_id_location id a =
  Array.find_index (fun x ->
      match x with
      | File (file_id, _) -> file_id = id
      | _ -> false) a |> Option.get

let move_file a location =
  let al = Array.length a in
  let new_array = ref a in
  match a.(location) with
  | Blank _ -> Array.sub a 0 (al - 1)
  | File (file_id, filesize) ->
    let file_moved = ref false in
    for i = 0 to location - 1 do
      if not !file_moved then
        match a.(i) with
        | Blank blanksize ->
          (if blanksize > filesize then
             (new_array := Array.concat [Array.sub a 0 i;
                                         [|File (file_id, filesize);Blank (blanksize - filesize)|];
                                         Array.sub a (i + 1) (al - i - 1)];
              new_array.contents.(location + 1) <- Blank filesize;
              file_moved := true)
           else if blanksize = filesize then
             (new_array := Array.concat [Array.sub a 0 i;
                                         [|File (file_id, filesize)|];
                                         Array.sub a (i + 1) (al - i - 1)];
              new_array.contents.(location) <- Blank filesize;
              file_moved := true)
          )
        | _ -> ()
    done;
    match (!new_array).(Array.length (!new_array) - 1) with
    | Blank _ -> Array.sub !new_array 0 (Array.length (!new_array) - 1) |> consolidate_blanks
    | _ -> !new_array |> consolidate_blanks

let () =
  let input_file = "inputs/day9.txt" in
  let memory_array =
    input_file
    |> read_lines
    |> List.hd
    |> explode
    |> List.map (fun x -> (int_of_char x) - 48)
    |> (fun l -> process_numbers l [] 0 false)
    |> List.filter_map (fun x ->
           match x with
           | Blank x -> if x = 0 then None else Some (Blank x)
           | File (a, b) -> Some (File (a, b)))
    |> Array.of_list
  in
  let memory_array_ref = ref (Array.copy memory_array) in
  while not (check_array_sorted !memory_array_ref) do
    memory_array_ref := fill_blank_element !memory_array_ref
  done;
  let total = ref 0 in
  let index = ref 0 in
  for i = 0 to (Array.length !memory_array_ref) - 1 do
    match (!memory_array_ref).(i) with
    | File (id, size) ->
       for j = (!index) to (!index + size - 1) do
         total := !total + j * id
       done;
       index := !index + size
    | Blank _ -> ()
  done;
  !total |> string_of_int |> print_endline;
  let max_file_id =
    let al = Array.length memory_array in
    match memory_array.(al - 1) with
    | File (id, _) -> id
    | _ -> -1
  in
  let moved_memory_array_ref = ref memory_array in
  for i = 0 to max_file_id do
    let current_id = max_file_id - i in
    moved_memory_array_ref := move_file !moved_memory_array_ref (find_id_location current_id !moved_memory_array_ref)
  done;
  let total = ref 0 in
  let index = ref 0 in
  for i = 0 to (Array.length !moved_memory_array_ref) - 1 do
    match (!moved_memory_array_ref).(i) with
    | File (id, size) ->
       for j = (!index) to (!index + size - 1) do
         total := !total + j * id
       done;
       index := !index + size
    | Blank n -> index := !index + n
  done;
  !total |> string_of_int |> print_endline;
