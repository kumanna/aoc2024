open Utils

module Computer = struct

  type t =
    {
      register_a : int ;
      register_b : int ;
      register_c : int ;
      instructions : int array ;
      instruction_pointer : int ;
      output : string ;
    }

  let create ra rb rc ilist =
    {
      register_a = ra ;
      register_b = rb ;
      register_c = rc ;
      instructions = ilist
                     |> String.split_on_char ','
                     |> List.map int_of_string
                     |> Array.of_list ;
      instruction_pointer = 0 ;
      output = "" ;
    }

  let rec pow n a =
    if a = 0 then 1 else
    if a = 1 then n else
      n * (pow n (a - 1))

  let process_instruction state =
    if state.instruction_pointer >= Array.length state.instructions then
      state
    else
      let next_number = state.instructions.(state.instruction_pointer + 1) in
      let get_combo_operand n =
        if 0 <= n && n <= 3 then n
        else if n = 4 then state.register_a
        else if n = 5 then state.register_b
        else if n = 6 then state.register_c
        else failwith "Illegal program!" in
      match state.instructions.(state.instruction_pointer) with
      | 0 ->
        { state with instruction_pointer = state.instruction_pointer + 2;
                     register_a = state.register_a / (pow 2 (get_combo_operand next_number)); }
      | 1 ->
        { state with instruction_pointer = state.instruction_pointer + 2;
                     register_b = next_number lxor state.register_b; }
      | 2 ->
        { state with instruction_pointer = state.instruction_pointer + 2;
                     register_b = (get_combo_operand next_number) land 7; }
      | 3 ->
        if state.register_a = 0 then
          { state with instruction_pointer = state.instruction_pointer + 2 }
        else
          { state with instruction_pointer = next_number }
      | 4 ->
        { state with instruction_pointer = state.instruction_pointer + 2;
                     register_b = state.register_c lxor state.register_b; }
      | 5 ->
        { state with instruction_pointer = state.instruction_pointer + 2;
                     output = state.output ^ (string_of_int ((get_combo_operand next_number) land 7)) ^ "," }
      | 6 ->
        { state with instruction_pointer = state.instruction_pointer + 2;
                     register_b = state.register_a / (pow 2 (get_combo_operand next_number)); }
      | 7 ->
        { state with instruction_pointer = state.instruction_pointer + 2;
                     register_c = state.register_a / (pow 2 (get_combo_operand next_number)); }
      | _ -> failwith "Illegal instruction!"

end

let () =
  let c = Computer.create 729 0 0 "0,1,5,4,3,0" in
  let c_ = ref c in
  while (!c_).instruction_pointer <= (Array.length (!c_).instructions - 2) do
    c_ := Computer.process_instruction !c_
  done;
  (!c_).output
  |> (fun x -> String.sub x 0 (String.length x - 1))
  |> print_endline;
  let input_file = "inputs/day17.txt"
  in
  let input_array =
    input_file |> read_lines
    |> Array.of_list in
  let ra = String.split_on_char ' ' input_array.(0) |> List.rev |> List.hd |> int_of_string in
  let rb = String.split_on_char ' ' input_array.(1) |> List.rev |> List.hd |> int_of_string in
  let rc = String.split_on_char ' ' input_array.(2) |> List.rev |> List.hd |> int_of_string in
  let p = String.split_on_char ' ' input_array.(4) |> List.rev |> List.hd in
  let c = Computer.create ra rb rc p in
  let c_ = ref c in
  while (!c_).instruction_pointer <= (Array.length (!c_).instructions - 2) do
    c_ := Computer.process_instruction !(c_)
  done;
  (!c_).output
  |> (fun x -> String.sub x 0 (String.length x - 1))
  |> print_endline;
