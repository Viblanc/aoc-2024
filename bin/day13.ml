type claw_machine = { a : int * int; b : int * int; prize : int * int }

let init = { a = (-1, 1); b = (-1, 1); prize = (-1, -1) }

let rec parse machine acc lines =
  let parse_stuff m line =
    let s = List.hd (String.split_on_char ':' line) in
    match s with
    | "Button A" ->
        Scanf.sscanf line "Button A: X+%d, Y+%d" (fun x y ->
            { m with a = (x, y) })
    | "Button B" ->
        Scanf.sscanf line "Button B: X+%d, Y+%d" (fun x y ->
            { m with b = (x, y) })
    | "Prize" ->
        Scanf.sscanf line "Prize: X=%d, Y=%d" (fun x y ->
            { m with prize = (x, y) })
    | _ -> m
  in
  match lines with
  | [] -> machine :: acc
  | "" :: tl -> parse init (machine :: acc) tl
  | l :: tl -> parse (parse_stuff machine l) acc tl

let claw_machines =
  In_channel.with_open_bin "inputs/13.txt" In_channel.input_lines
  |> parse init []

let solve_equation { a = xa, ya; b = xb, yb; prize = xp, yp } =
  let n = ((xp * yb) - (yp * xb)) / ((xa * yb) - (ya * xb)) in
  let k = (xp - (n * xa)) / xb in
  if (n * xa) + (k * xb) = xp && (n * ya) + (k * yb) = yp then Some (n, k)
  else None

let part_1 =
  let solutions = List.filter_map solve_equation claw_machines in
  List.(fold_left ( + ) 0 (map (fun (a, b) -> (a * 3) + b) solutions))

let claw_machines =
  List.map
    (fun m ->
      let { prize = xp, yp; _ } = m in
      { m with prize = (xp + 10_000_000_000_000, yp + 10_000_000_000_000) })
    claw_machines

let part_2 =
  let solutions = List.filter_map solve_equation claw_machines in
  List.(fold_left ( + ) 0 (map (fun (a, b) -> (a * 3) + b) solutions))

let _ =
  Printf.printf "Solution for part 1: %d\n" part_1;
  Printf.printf "Solution for part 2: %d\n" part_2
