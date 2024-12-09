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

let () =
  let input_file = "inputs/day9.txt" in
  let memory_array = ref
    (input_file
    |> read_lines
    |> List.hd
    |> explode
    |> List.map (fun x -> (int_of_char x) - 48)
    |> (fun l -> process_numbers l [] 0 false)
    |> List.filter_map (fun x ->
           match x with
           | Blank x -> if x = 0 then None else Some (Blank x)
           | File (a, b) -> Some (File (a, b)))
    |> Array.of_list)
  in
  while not (check_array_sorted !memory_array) do
    memory_array := fill_blank_element !memory_array
  done;
  let total = ref 0 in
  let index = ref 0 in
  for i = 0 to (Array.length !memory_array) - 1 do
    match (!memory_array).(i) with
    | File (id, size) ->
       for j = (!index) to (!index + size - 1) do
         total := !total + j * id
       done;
       index := !index + size
    | Blank _ -> ()
  done;
  !total |> string_of_int |> print_endline;
