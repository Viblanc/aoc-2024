type instruction = Do | Dont | Mul of int * int

let to_instr = function
  | "do()" -> Do
  | "don't()" -> Dont
  | s -> Scanf.sscanf s "mul(%d,%d)" (fun x y -> Mul (x, y))

let input = In_channel.with_open_bin "inputs/03.txt" In_channel.input_lines

let search_forward_opt rx s i =
  try Some (Str.search_forward rx s i) with Not_found -> None

let parse rx s =
  let rec do_parse acc i =
    match search_forward_opt rx s i with
    | Some _ ->
        let matched_string = Str.matched_string s in
        do_parse (to_instr matched_string :: acc) (Str.match_end ())
    | None -> acc
  in
  List.rev (do_parse [] 0)

let part_1 =
  let rx = Str.regexp {|mul([0-9]+,[0-9]+)|} in
  List.map (parse rx) input
  |> List.flatten
  |> List.fold_left
       (fun acc instr -> acc + match instr with Mul (x, y) -> x * y | _ -> 0)
       0

let part_2 =
  let rx = Str.regexp {|do()\|don't()\|mul([0-9]+,[0-9]+)|} in
  List.map (parse rx) input
  |> List.flatten
  |> List.fold_left
       (fun (enabled, sum) instr ->
         match instr with
         | Do -> (true, sum)
         | Dont -> (false, sum)
         | Mul (x, y) ->
             if enabled then (enabled, sum + (x * y)) else (enabled, sum))
       (true, 0)
  |> snd

let _ =
  Printf.printf "Solution for part 1 : %d\n" part_1;
  Printf.printf "Solution for part 2 : %d\n" part_2
