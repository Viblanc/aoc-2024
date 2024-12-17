type dir = N | E | S | W

let opposing = function N -> S | S -> N | E -> W | W -> E
let rotate_clockwise = function N -> E | E -> S | S -> W | W -> N
let rotate_counter_clockwise = function N -> W | W -> S | S -> E | E -> N

module Pos = struct
  type t = int * int

  let compare (x, y) (x', y') =
    match Int.compare x x' with 0 -> Int.compare y y' | n -> n
end

module Node = struct
  type t = Pos.t * dir

  let compare (p, d) (p', d') =
    match Pos.compare p p' with 0 -> Stdlib.compare d d' | n -> n
end

module WeightedNodes = struct
  type t = Node.t * int

  let compare (n, w) (n', w') =
    match compare w w' with 0 -> Node.compare n n' | k -> k
end

module Positions = Set.Make (Pos)
module Nodes = Set.Make (Node)
module Prio = Set.Make (WeightedNodes)
module Costs = Map.Make (Node)

let grid =
  In_channel.with_open_bin "inputs/16.txt" In_channel.input_lines
  |> List.map (fun s -> String.to_seq s |> Array.of_seq)
  |> Array.of_list

let height, width = (Array.length grid, Array.length grid.(0))

let find_target target =
  Array.find_mapi
    (fun i arr ->
      match Array.find_index (fun c -> c = target) arr with
      | Some j -> Some (i, j)
      | _ -> None)
    grid
  |> Option.get

let step (x, y) = function
  | N -> (x - 1, y)
  | E -> (x, y + 1)
  | S -> (x + 1, y)
  | W -> (x, y - 1)

let neighbors (pos, dir) =
  List.filter_map
    (fun (pos, dir, c) ->
      let x, y = pos in
      match grid.(x).(y) with '#' -> None | _ -> Some ((pos, dir), c))
    [
      (step pos dir, dir, 1);
      (pos, rotate_clockwise dir, 1000);
      (pos, rotate_counter_clockwise dir, 1000);
    ]

let update_costs costs prio parent node cost =
  let c = Costs.find parent costs + cost in
  let c' = Option.value ~default:max_int (Costs.find_opt node costs) in
  if c' <= c then (costs, prio)
  else (Costs.add node c costs, Prio.add (node, c) prio)

let rec dijkstra (costs, prio) =
  if Prio.is_empty prio then costs
  else
    let cur, cost = Prio.min_elt prio in
    dijkstra
      (List.fold_left
         (fun (costs, prio) (n, c) -> update_costs costs prio cur n c)
         (costs, Prio.remove (cur, cost) prio)
         (neighbors cur))

let start = find_target 'S'
let finish = find_target 'E'

let part_1 =
  let prio = Prio.singleton ((start, E), 0) in
  let costs = Costs.singleton (start, E) 0 in
  Costs.fold
    (fun (pos, _) c acc -> if pos = finish then min c acc else acc)
    (dijkstra (costs, prio))
    max_int

let neighbors (pos, dir) =
  List.filter_map
    (fun (pos, dir, c) ->
      let x, y = pos in
      match grid.(x).(y) with '#' -> None | _ -> Some ((pos, dir), c))
    [
      (step pos dir, dir, 1);
      (step pos (opposing dir), dir, 1);
      (pos, rotate_clockwise dir, 1000);
      (pos, rotate_counter_clockwise dir, 1000);
    ]

let rec bfs costs seen todo =
  match todo with
  | [] -> seen
  | ((pos, dir), cost) :: tl ->
      let seen = Positions.add pos seen in
      let ns =
        List.filter
          (fun ((p, d), c) -> cost - c = Costs.find (p, d) costs)
          (neighbors (pos, dir))
        |> List.map (fun ((p, d), _) -> ((p, d), Costs.find (p, d) costs))
      in
      let costs =
        Costs.add_seq
          (List.map (fun (n, _) -> (n, max_int)) ns |> List.to_seq)
          costs
      in
      bfs costs seen (tl @ ns)

let part_2 =
  let cost = part_1 in
  let prio = Prio.singleton ((start, E), 0) in
  let costs = Costs.singleton (start, E) 0 in
  let costs = dijkstra (costs, prio) in
  let seen = Positions.empty in
  let todo =
    Costs.fold
      (fun (p, d) c acc ->
        if p = finish && c = cost then ((p, d), c) :: acc else acc)
      costs []
  in
  Positions.cardinal (bfs costs seen todo)

let _ =
  Printf.printf "Solution for part 1: %d\n" part_1;
  Printf.printf "Solution for part 2: %d\n" part_2
