module Position = struct
  type t = int * int

  let compare (x, y) (x', y') =
    match Int.compare x x' with 0 -> Int.compare y y' | n -> n
end

module Grid = Map.Make (Position)
module Antennas = Map.Make (Char)
module Antinodes = Set.Make (Position)

let grid =
  In_channel.with_open_bin "inputs/08.txt" In_channel.input_lines
  |> List.mapi (fun i s ->
         String.to_seq s |> Seq.mapi (fun j c -> ((i, j), c)) |> List.of_seq)
  |> List.flatten |> Grid.of_list

let antennas =
  let rec pair_antennas acc l =
    let rec aux elt acc = function
      | [] -> acc
      | hd :: tl -> aux elt ((elt, hd) :: acc) tl
    in
    match l with
    | [] -> List.flatten acc
    | hd :: tl -> pair_antennas (aux hd [] tl :: acc) tl
  in
  let antenna_pos =
    Grid.filter (fun _ -> function '.' -> false | _ -> true) grid
  in
  Grid.fold
    (fun pos a acc ->
      match Antennas.find_opt a acc with
      | None -> Antennas.add a [ pos ] acc
      | Some al -> Antennas.add a (pos :: al) acc)
    antenna_pos Antennas.empty
  |> Antennas.map (pair_antennas [])

let add_antinode set (a1, a2) =
  let (x, y), (x', y') = (a1, a2) in
  let dx, dy = (x - x', y - y') in
  List.fold_left
    (fun acc pos -> if Grid.mem pos grid then Antinodes.add pos acc else acc)
    set
    [ (x + dx, y + dy); (x' - dx, y' - dy) ]

let part_1 =
  Antennas.fold
    (fun _ al acc -> List.fold_left add_antinode acc al)
    antennas Antinodes.empty
  |> Antinodes.cardinal

let add_antinode set (a1, a2) =
  let (x, y), (x', y') = (a1, a2) in
  let dx, dy = (x - x', y - y') in
  let generate_points f (x, y) =
    Seq.iterate f (x, y) |> Seq.take_while (fun p -> Grid.mem p grid)
  in
  let s1 = generate_points (fun (x, y) -> (x + dx, y + dy)) (x, y) in
  let s2 = generate_points (fun (x, y) -> (x - dx, y - dy)) (x', y') in
  Antinodes.(add_seq s1 (add_seq s2 set))

let part_2 =
  Antennas.fold
    (fun _ al acc -> List.fold_left add_antinode acc al)
    antennas Antinodes.empty
  |> Antinodes.cardinal

let _ =
  Printf.printf "Solution for part 1 : %d\n" part_1;
  Printf.printf "Solution for part 2 : %d\n" part_2
