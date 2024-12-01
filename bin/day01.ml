let input =
  In_channel.with_open_bin "./inputs/01.txt" In_channel.input_lines
  |> List.map (fun s -> Scanf.sscanf s "%d %d" (fun a b -> (a, b)))
  |> List.split

let left, right = input

let part_1 =
  let left = List.sort Int.compare left in
  let right = List.sort Int.compare right in
  List.combine left right
  |> List.map (fun (l, r) -> abs (l - r))
  |> List.fold_left ( + ) 0

module Scores = Map.Make (Int)

let scores =
  List.fold_left
    (fun acc n ->
      Scores.update n
        (fun opt -> match opt with Some i -> Some (i + 1) | None -> Some 1)
        acc)
    Scores.empty right

let part_2 =
  List.map
    (fun n -> match Scores.find_opt n scores with Some i -> n * i | None -> 0)
    left
  |> List.fold_left ( + ) 0

let _ =
  Printf.printf "Solution for part 1 : %d\n" part_1;
  Printf.printf "Solution for part 2 : %d\n" part_2
