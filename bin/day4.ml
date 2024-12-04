open Utils

let explode s = List.init (String.length s) (String.get s)

let find_xmas grid =
  let rowmax = Array.length grid in
  let colmax = Array.length grid.(0) in
  let find_xmas_helper row col =
    let n1 = if row >= 3 &&
                (grid.(row - 1).(col) = 'M') &&
                (grid.(row - 2).(col) = 'A') &&
                (grid.(row - 3).(col) = 'S')
      then 1 else 0 in
    let n2 = if row < rowmax - 3 &&
                (grid.(row + 1).(col) = 'M') &&
                (grid.(row + 2).(col) = 'A') &&
                (grid.(row + 3).(col) = 'S')
      then 1 else 0 in
    let n3 = if col >= 3 &&
                (grid.(row).(col - 1) = 'M') &&
                (grid.(row).(col - 2) = 'A') &&
                (grid.(row).(col - 3) = 'S')
      then 1 else 0 in
    let n4 = if col < colmax - 3 &&
                (grid.(row).(col + 1) = 'M') &&
                (grid.(row).(col + 2) = 'A') &&
                (grid.(row).(col + 3) = 'S')
      then 1 else 0 in
    let n5 = if row >= 3 && col >= 3 &&
                (grid.(row - 1).(col - 1) = 'M') &&
                (grid.(row - 2).(col - 2) = 'A') &&
                (grid.(row - 3).(col - 3) = 'S')
      then 1 else 0 in
    let n6 = if row >= 3 && col < colmax - 3 &&
                (grid.(row - 1).(col + 1) = 'M') &&
                (grid.(row - 2).(col + 2) = 'A') &&
                (grid.(row - 3).(col + 3) = 'S')
      then 1 else 0 in
    let n7 = if row < rowmax - 3 && col < colmax - 3 &&
                (grid.(row + 1).(col + 1) = 'M') &&
                (grid.(row + 2).(col + 2) = 'A') &&
                (grid.(row + 3).(col + 3) = 'S')
      then 1 else 0 in
    let n8 = if row < rowmax - 3 && col >= 3 &&
                (grid.(row + 1).(col - 1) = 'M') &&
                (grid.(row + 2).(col - 2) = 'A') &&
                (grid.(row + 3).(col - 3) = 'S')
      then 1 else 0 in
    n1 + n2 + n3 + n4 + n5 + n6 + n7 + n8
  in
  let xmascount = ref 0 in
  for row = 0 to rowmax - 1 do
    for col = 0 to colmax - 1 do
      if grid.(row).(col) = 'X' then
        xmascount := !xmascount + (find_xmas_helper row col)
    done
  done;
  !xmascount

let find_xmas_cross grid =
  let rowmax = Array.length grid in
  let colmax = Array.length grid.(0) in
  let find_xmas_cross_helper row col =
    if row > 0 && row < rowmax - 1 && col > 0 && col < colmax - 1 then
      if (grid.(row - 1).(col - 1) = 'M' && grid.(row + 1).(col + 1) = 'S' ||
         grid.(row + 1).(col + 1) = 'M' && grid.(row - 1).(col - 1) = 'S') &&
         (grid.(row - 1).(col + 1) = 'M' && grid.(row + 1).(col - 1) = 'S' ||
          grid.(row + 1).(col - 1) = 'M' && grid.(row - 1).(col + 1) = 'S')
      then
        1 else 0
    else 0
  in
  let xmascount = ref 0 in
  for row = 0 to rowmax - 1 do
    for col = 0 to colmax - 1 do
      if grid.(row).(col) = 'A' then
        xmascount := !xmascount + (find_xmas_cross_helper row col)
    done
  done;
  !xmascount

let () =
    let input_file = "inputs/day4.txt" in
    input_file
    |> read_lines
    |> List.filter (fun x -> String.length x > 1)
    |> List.map explode
    |> List.map Array.of_list
    |> Array.of_list
    |> find_xmas
    |> string_of_int
    |> print_endline;
    input_file
    |> read_lines
    |> List.filter (fun x -> String.length x > 1)
    |> List.map explode
    |> List.map Array.of_list
    |> Array.of_list
    |> find_xmas_cross
    |> string_of_int
    |> print_endline;
