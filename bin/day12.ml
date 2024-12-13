module Position = struct
  type t = int * int

  let neighbors (x, y) = [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]

  let compare (x, y) (x', y') =
    match Int.compare x x' with 0 -> Int.compare y y' | n -> n
end

module Seen = Set.Make (Position)

let grid =
  In_channel.with_open_bin "inputs/12.txt" In_channel.input_lines
  |> List.map (fun s -> String.to_seq s |> Array.of_seq)
  |> Array.of_list

let rec flood seen plots target = function
  | [] -> plots
  | (x, y) :: tl -> (
      if Seen.mem (x, y) seen then flood seen plots target tl
      else
        match try Some grid.(x).(y) with _ -> None with
        | Some c when c = target ->
            flood
              (Seen.add (x, y) seen)
              ((x, y) :: plots) target
              (Position.neighbors (x, y) @ tl)
        | Some _ -> flood seen plots target tl
        | _ -> flood seen plots target tl)

let height, width = (Array.length grid, Array.length grid.(0))

let vertices =
  let xs = List.init height Fun.id in
  let ys = List.init width Fun.id in
  List.map (fun x -> List.map (fun y -> (x, y)) ys) xs |> List.flatten

let plots =
  let rec aux acc seen = function
    | [] -> acc
    | v :: vs ->
        if Seen.mem v seen then aux acc seen vs
        else
          let vx, vy = v in
          let target = grid.(vx).(vy) in
          let plots = flood Seen.empty [] target [ v ] in
          aux (plots :: acc) (Seen.add_seq (List.to_seq plots) seen) vs
  in
  aux [] Seen.empty vertices

let perimeter l =
  let fences v =
    Position.neighbors v
    |> List.filter (fun v -> not (List.mem v l))
    |> List.map (fun v' -> (v, v'))
  in
  let rec aux = function [] -> [] | v :: vs -> fences v @ aux vs in
  aux l

let part_1 =
  List.map (fun l -> List.length l * List.length (perimeter l)) plots
  |> List.fold_left ( + ) 0

let count_sides l =
  let module S = Set.Make (struct
    type t = Position.t * Position.t

    let compare p1 p2 = Stdlib.compare p1 p2
  end) in
  let per = perimeter l in
  let rec aux acc = function
    | [] -> acc
    | ((x, y), (x', y')) :: tl ->
        let keep =
          List.map
            (fun (dx, dy) -> ((x + dx, y + dy), (x' + dx, y' + dy)))
            [ (0, 1); (1, 0) ]
          |> List.fold_left (fun keep x -> keep && not (List.mem x per)) true
        in
        if keep then aux (S.add ((x, y), (x', y')) acc) tl else aux acc tl
  in
  S.cardinal (aux S.empty per)

let part_2 =
  List.map (fun l -> List.length l * count_sides l) plots
  |> List.fold_left ( + ) 0

let _ =
  Printf.printf "Solution for part 1 : %d\n" part_1;
  Printf.printf "Solution for part 2 : %d\n" part_2
