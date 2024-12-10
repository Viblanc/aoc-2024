module Position = struct
  type t = int * int

  let neighbors (x, y) = [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]

  let compare (x, y) (x', y') =
    match Int.compare x x' with 0 -> Int.compare y y' | n -> n
end

module Grid = Map.Make (Position)
module Positions = Set.Make (Position)

let grid =
  In_channel.with_open_bin "inputs/10.txt" In_channel.input_lines
  |> List.mapi (fun i s ->
         String.to_seq s
         |> Seq.mapi (fun j c -> ((i, j), int_of_char c - int_of_char '0'))
         |> List.of_seq)
  |> List.flatten |> Grid.of_list

let trailheads =
  Grid.(
    fold
      (fun pos _ acc -> pos :: acc)
      (filter (fun _ -> function 0 -> true | _ -> false) grid)
      [])

let rec calculate_score grid nines pos =
  let increase_by_one height pos =
    match Grid.find_opt pos grid with Some h -> h = height + 1 | _ -> false
  in
  let height = Grid.find pos grid in
  if height = 9 then Positions.add pos nines
  else
    let neighbors =
      List.filter (increase_by_one height) (Position.neighbors pos)
    in
    match neighbors with
    | [] -> nines
    | _ -> List.fold_left (calculate_score grid) nines neighbors

let part_1 =
  List.map (calculate_score grid Positions.empty) trailheads
  |> List.map Positions.cardinal
  |> List.fold_left ( + ) 0

let rec calculate_rating grid pos =
  let increase_by_one height pos =
    match Grid.find_opt pos grid with Some h -> h = height + 1 | _ -> false
  in
  let height = Grid.find pos grid in
  if height = 9 then 1
  else
    let neighbors =
      List.filter (increase_by_one height) (Position.neighbors pos)
    in
    match neighbors with
    | [] -> 0
    | _ -> List.(fold_left ( + ) 0 (map (calculate_rating grid) neighbors))

let part_2 =
  List.map (calculate_rating grid) trailheads |> List.fold_left ( + ) 0

let _ =
  Printf.printf "Solution for part 1 : %d\n" part_1;
  Printf.printf "Solution for part 2 : %d\n" part_2
