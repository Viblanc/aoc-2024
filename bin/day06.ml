type dir = U | D | L | R

let next_dir = function U -> R | D -> L | L -> U | R -> D

module PosDir = struct
  type t = (int * int) * dir

  let move (x, y) = function
    | U -> (x - 1, y)
    | D -> (x + 1, y)
    | L -> (x, y - 1)
    | R -> (x, y + 1)

  let compare ((x, y), d) ((x', y'), d') =
    match Int.compare x x' with
    | 0 -> ( match Int.compare y y' with 0 -> compare d d' | n -> n)
    | n -> n
end

module Positions = Set.Make (struct
  type t = int * int

  let compare (x, y) (x', y') =
    match Int.compare x x' with 0 -> Int.compare y y' | n -> n
end)

module PositionsDir = Set.Make (PosDir)

let input =
  In_channel.with_open_bin "inputs/06.txt" In_channel.input_lines
  |> List.map (fun s -> String.to_seq s |> Array.of_seq)
  |> Array.of_list

let start_pos =
  Array.find_mapi
    (fun i row ->
      Array.find_mapi
        (fun j c -> match c with '^' -> Some (i, j) | _ -> None)
        row)
    input
  |> Option.get

let is_out_of_bounds (x, y) =
  let height, width = (Array.length input, Array.length input.(0)) in
  x < 0 || height <= x || y < 0 || width <= y

let rec move_guard seen dir current_pos =
  let x, y = PosDir.move current_pos dir in
  if is_out_of_bounds (x, y) then List.rev seen
  else
    match input.(x).(y) with
    | '#' -> move_guard seen (next_dir dir) current_pos
    | _ -> move_guard (((x, y), dir) :: seen) dir (x, y)

let guard_path =
  move_guard [ (start_pos, U) ] U start_pos |> List.map fst |> Positions.of_list

let part_1 = Positions.cardinal guard_path

let find_cycle obstacle_pos =
  let rec aux seen dir current_pos =
    let x, y = PosDir.move current_pos dir in
    if is_out_of_bounds (x, y) then false
    else if (x, y) = obstacle_pos then aux seen (next_dir dir) current_pos
    else
      match (PositionsDir.find_opt ((x, y), dir) seen, input.(x).(y)) with
      | Some _, _ -> true
      | _, '#' -> aux seen (next_dir dir) current_pos
      | _ -> aux (PositionsDir.add ((x, y), dir) seen) dir (x, y)
  in
  aux (PositionsDir.singleton (start_pos, U)) U start_pos

let part_2 =
  Positions.fold
    (fun pos acc -> match find_cycle pos with false -> acc | true -> acc + 1)
    guard_path 0

let _ =
  Printf.printf "Solution for part 1 : %d\n" part_1;
  Printf.printf "Solution for part 2 : %d\n" part_2
