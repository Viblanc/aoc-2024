module Pos = struct
  type t = int * int

  let compare (x, y) (x', y') =
    match Int.compare x x' with 0 -> Int.compare y y' | n -> n
end

module Node = struct
  type t = Pos.t * int

  let compare (p, d) (p', d') =
    match Pos.compare p p' with 0 -> Stdlib.compare d d' | n -> n
end

module Positions = Set.Make (Pos)
module Prio = Set.Make (Node)
module Dist = Map.Make (Pos)

let bytes_pos =
  let parse_line line = Scanf.sscanf line "%d,%d" (fun col row -> (row, col)) in
  In_channel.with_open_bin "inputs/18.txt" In_channel.input_lines
  |> List.map parse_line |> List.to_seq

let size = 71
let start = (0, 0)
let finish = (size - 1, size - 1)
let in_bounds (i, j) = 0 <= i && i < size && 0 <= j && j < size

let neighbours blocks (i, j) =
  List.filter
    (fun p -> in_bounds p && not (Positions.mem p blocks))
    [ (i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1) ]

let update_costs dists prio prev node dist =
  let d = Dist.find prev dists + dist in
  let d' = Option.value ~default:max_int (Dist.find_opt node dists) in
  if d' <= d then (dists, prio)
  else (Dist.add node d dists, Prio.add (node, d) prio)

let rec dijkstra blocks (dists, prio) =
  if Prio.is_empty prio then dists
  else
    let pos, dist = Prio.min_elt prio in
    let ns = neighbours blocks pos in
    dijkstra
      Positions.(union (of_list ns) blocks)
      (List.fold_left
         (fun (dists, prio) n -> update_costs dists prio pos n 1)
         (dists, Prio.remove (pos, dist) prio)
         ns)

let find_min_steps n =
  let costs = Dist.singleton start 0 in
  let prio = Prio.singleton (start, 0) in
  let blocks = Positions.of_seq (Seq.take n bytes_pos) in
  Dist.fold
    (fun pos c acc -> if pos = finish then min c acc else acc)
    (dijkstra blocks (costs, prio))
    max_int

let part_1 = find_min_steps 1024

let rec find_blocking_byte l r bytes_pos =
  if l >= r then List.(nth (of_seq bytes_pos) l)
  else
    let m = (l + r) / 2 in
    if find_min_steps m = max_int then find_blocking_byte l (m - 1) bytes_pos
    else find_blocking_byte (m + 1) r bytes_pos

let part_2 = find_blocking_byte 0 (Seq.length bytes_pos - 1) bytes_pos

let _ =
  Printf.printf "Solution for part 1: %d\n" part_1;
  Printf.printf "Solution for part 2: (%d,%d)" (snd part_2) (fst part_2)
