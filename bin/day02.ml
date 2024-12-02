let input =
  In_channel.with_open_bin "./inputs/02.txt" In_channel.input_lines
  |> List.map (fun s -> String.split_on_char ' ' s |> List.map int_of_string)

let rec all_increasing = function
  | x :: (y :: _ as t) ->
      let diff = y - x in
      if 1 <= diff && diff <= 3 then all_increasing t else false
  | _ -> true

let all_decreasing l = all_increasing (List.rev l)
let is_safe l = all_increasing l || all_decreasing l
let part_1 = List.filter is_safe input |> List.length

let is_safe2 l =
  let rec aux left right =
    match right with
    | h :: t ->
        if is_safe (List.rev_append left t) then true else aux (h :: left) t
    | _ -> false
  in
  aux [] l

let part_2 = List.filter is_safe2 input |> List.length

let _ =
  Printf.printf "Solution for part 1 : %d\n" part_1;
  Printf.printf "Solution for part 2 : %d\n" part_2
