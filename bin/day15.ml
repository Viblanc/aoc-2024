type dir = U | D | L | R
type tile = Wall | Box | Robot | Empty

module Position = struct
  type t = int * int

  let move dir (x, y) =
    match dir with
    | U -> (x - 1, y)
    | D -> (x + 1, y)
    | L -> (x, y - 1)
    | R -> (x, y + 1)

  let compare (x, y) (x', y') =
    match Int.compare x x' with 0 -> Int.compare y y' | n -> n
end

module Grid = Map.Make (Position)

let parse_grid s =
  let parse_tile = function
    | '#' -> Wall
    | 'O' -> Box
    | '@' -> Robot
    | _ -> Empty
  in
  String.split_on_char '\n' s
  |> List.mapi (fun i s ->
         String.to_seq s
         |> Seq.mapi (fun j c -> ((i, j), parse_tile c))
         |> List.of_seq)
  |> List.flatten |> Grid.of_list

let parse_moves s =
  let parse_move line =
    let chars = String.to_seq line |> List.of_seq in
    let parse_move = function '^' -> U | 'v' -> D | '<' -> L | _ -> R in
    let rec aux acc = function
      | [] | [ '\n' ] -> List.rev acc
      | c :: cs -> aux (parse_move c :: acc) cs
    in
    aux [] chars
  in
  String.split_on_char '\n' s |> List.map parse_move |> List.flatten

let list_to_tuple = function [ a; b ] -> (a, b) | _ -> failwith "not a tuple"

let raw_grid, raw_moves =
  In_channel.with_open_bin "inputs/15.txt" In_channel.input_all
  |> Str.(split (regexp "\n\n"))
  |> list_to_tuple

let grid, moves = (parse_grid raw_grid, parse_moves raw_moves)

let robot_pos =
  Grid.fold
    (fun pos tile acc -> match tile with Robot -> pos | _ -> acc)
    grid (-1, -1)

let rec simulate_moves robot_pos grid moves =
  let update_robot old_pos pos grid =
    let tile = Grid.find old_pos grid in
    Grid.(add old_pos Empty (add pos tile grid))
  in
  let rec move_robot (x, y) dir grid =
    let nx, ny = Position.move dir (x, y) in
    match Grid.find (nx, ny) grid with
    | Robot -> failwith "something went terribly wrong"
    | Wall -> ((x, y), grid)
    | Empty -> ((nx, ny), update_robot (x, y) (nx, ny) grid)
    | Box ->
        let (bx, by), grid = move_robot (nx, ny) dir grid in
        if (bx, by) = (nx, ny) then ((x, y), grid)
        else ((nx, ny), update_robot (x, y) (nx, ny) grid)
  in
  match moves with
  | [] -> grid
  | m :: ms ->
      let robot_pos, grid = move_robot robot_pos m grid in
      simulate_moves robot_pos grid ms

let part_1 =
  let grid = simulate_moves robot_pos grid moves in
  Grid.fold
    (fun (x, y) tile acc ->
      match tile with Box -> (x * 100) + y + acc | _ -> acc)
    grid 0

type tile' = Wall | LBox | RBox | Robot | Empty

let parse_grid s =
  let parse_tile = function
    | '#' -> (Wall, Wall)
    | 'O' -> (LBox, RBox)
    | '@' -> (Robot, Empty)
    | _ -> (Empty, Empty)
  in
  String.split_on_char '\n' s
  |> List.mapi (fun i s ->
         String.to_seqi s
         |> Seq.flat_map (fun (j, c) ->
                let t1, t2 = parse_tile c in
                List.to_seq [ ((i, 2 * j), t1); ((i, (2 * j) + 1), t2) ])
         |> List.of_seq)
  |> List.flatten |> Grid.of_list

let grid, moves = (parse_grid raw_grid, parse_moves raw_moves)

let robot_pos =
  Grid.fold
    (fun pos tile acc -> match tile with Robot -> pos | _ -> acc)
    grid (-1, -1)

let rec simulate_moves robot_pos grid moves =
  let update_robot old_pos pos grid =
    let tile = Grid.find old_pos grid in
    Grid.(add old_pos Empty (add pos tile grid))
  in
  let rec move_box (lx, ly) dir grid =
    let rx, ry = (lx, ly + 1) in
    let (lx', ly'), g =
      match dir with
      | L ->
          let l', g = move_robot (lx, ly) dir grid in
          let r', g =
            if l' <> (lx, ly) then move_robot (rx, ry) dir g
            else ((lx, ly), grid)
          in
          if r' <> (rx, ry) then (l', g) else ((lx, ly), grid)
      | _ ->
          let r', g = move_robot (rx, ry) dir grid in
          let l', g =
            if r' <> (rx, ry) then move_robot (lx, ly) dir g
            else ((lx, ly), grid)
          in
          if l' <> (lx, ly) then (l', g) else ((lx, ly), grid)
    in
    ((lx', ly'), g)
  and move_robot (x, y) dir grid =
    let nx, ny = Position.move dir (x, y) in
    match Grid.find (nx, ny) grid with
    | Robot -> failwith "something went terribly wrong"
    | Wall -> ((x, y), grid)
    | Empty -> ((nx, ny), update_robot (x, y) (nx, ny) grid)
    | LBox ->
        let (bx, by), grid = move_box (nx, ny) dir grid in
        if (bx, by) = (nx, ny) then ((x, y), grid)
        else ((nx, ny), update_robot (x, y) (nx, ny) grid)
    | RBox ->
        let (bx, by), grid = move_box (nx, ny - 1) dir grid in
        if (bx, by) = (nx, ny - 1) then ((x, y), grid)
        else ((nx, ny), update_robot (x, y) (nx, ny) grid)
  in
  match moves with
  | [] -> grid
  | m :: ms ->
      let robot_pos, grid = move_robot robot_pos m grid in
      simulate_moves robot_pos grid ms

let part_2 =
  let grid = simulate_moves robot_pos grid moves in
  Grid.fold
    (fun (x, y) tile acc ->
      match tile with LBox -> (x * 100) + y + acc | _ -> acc)
    grid 0

let _ =
  Printf.printf "Solution for part 1: %d\n" part_1;
  Printf.printf "Solution for part 2: %d\n" part_2
