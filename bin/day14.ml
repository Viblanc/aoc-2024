let parse_line line =
  Scanf.sscanf line "p=%d,%d v=%d,%d" (fun px py vx vy -> ((px, py), (vx, vy)))

let robots =
  In_channel.with_open_bin "inputs/14.txt" In_channel.input_lines
  |> List.map parse_line

let move_robots ~width ~height ~seconds robots =
  let move_robot (pos, vel) =
    let px, py = pos in
    let vx, vy = vel in
    ( ( (px + ((vx + width) * seconds)) mod width,
        (py + ((vy + height) * seconds)) mod height ),
      vel )
  in
  List.map move_robot robots

let safety_factor w h robots =
  let quadrants =
    [
      List.filter (fun ((x, y), _) -> x < w / 2 && y < h / 2) robots;
      List.filter (fun ((x, y), _) -> x > w / 2 && y < h / 2) robots;
      List.filter (fun ((x, y), _) -> x < w / 2 && y > h / 2) robots;
      List.filter (fun ((x, y), _) -> x > w / 2 && y > h / 2) robots;
    ]
  in
  List.(fold_left ( * ) 1 (List.map List.length quadrants))

let part_1 =
  let w, h = (101, 103) in
  let robots = move_robots ~width:w ~height:h ~seconds:100 robots in
  safety_factor w h robots

(* I just made a wild guess that robots wouldn't overlap to picture a Christmas tree *)
let is_xmas_tree robots =
  let module S = Set.Make (struct
    type t = int * int

    let compare (x, y) (x', y') =
      match Int.compare x x' with 0 -> Int.compare y y' | n -> n
  end) in
  let positions = List.map fst robots in
  let set = S.of_list positions in
  List.length positions = S.cardinal set

let find_xmas_tree w h robots =
  let m = Hashtbl.create 8192 in
  let rec find_cycle robots i =
    match Hashtbl.find_opt m robots with
    | None ->
        Hashtbl.add m robots i;
        find_cycle (move_robots ~width:w ~height:h ~seconds:1 robots) (i + 1)
    | Some _ ->
        Hashtbl.fold
          (fun rs target acc -> if is_xmas_tree rs then target else acc)
          m (-1)
  in
  find_cycle robots 0

let part_2 = find_xmas_tree 101 103 robots

let _ =
  Printf.printf "Solution for part 1: %d\n" part_1;
  Printf.printf "Solution for part 2: %d\n" part_2
