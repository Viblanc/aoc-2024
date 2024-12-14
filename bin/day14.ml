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

let quadrants w h robots =
  let f1 (x, y) = x < w / 2 && y < h / 2 in
  let f2 (x, y) = x > w / 2 && y < h / 2 in
  let f3 (x, y) = x < w / 2 && y > h / 2 in
  let f4 (x, y) = x > w / 2 && y > h / 2 in
  List.fold_left
    (fun (q1, q2, q3, q4) (p, _) ->
      match (f1 p, f2 p, f3 p, f4 p) with
      | true, _, _, _ -> (p :: q1, q2, q3, q4)
      | _, true, _, _ -> (q1, p :: q2, q3, q4)
      | _, _, true, _ -> (q1, q2, p :: q3, q4)
      | _, _, _, true -> (q1, q2, q3, p :: q4)
      | _ -> (q1, q2, q3, q4))
    ([], [], [], []) robots

let safety_factor w h robots =
  let q1, q2, q3, q4 = quadrants w h robots in
  List.(fold_left ( * ) 1 (List.map List.length [ q1; q2; q3; q4 ]))

let part_1 =
  let w, h = (101, 103) in
  let robots = move_robots ~width:w ~height:h ~seconds:100 robots in
  safety_factor w h robots

(* I previously made a wild guess that robots wouldn't overlap to picture a Christmas tree *)
(* The trick was to use the quadrants we previously calculated *)
(* If most pixels are gathered in a single quadrant, we've found our tree *)
let find_xmas_tree w h robots =
  let is_xmas_tree robots =
    let q1, q2, q3, q4 = quadrants w h robots in
    let qs = [ q1; q2; q3; q4 ] in
    let k = List.fold_left ( + ) 0 (List.map List.length qs) in
    List.find_opt
      (fun q ->
        let k' = List.length q in
        k' > k - k')
      qs
    |> Option.fold ~none:false ~some:(fun _ -> true)
  in
  let rec repeat robots i =
    if is_xmas_tree robots then i
    else repeat (move_robots ~width:w ~height:h ~seconds:1 robots) (i + 1)
  in
  repeat robots 0

let part_2 = find_xmas_tree 101 103 robots

let _ =
  Printf.printf "Solution for part 1: %d\n" part_1;
  Printf.printf "Solution for part 2: %d\n" part_2
