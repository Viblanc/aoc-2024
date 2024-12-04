(* Top, bottom, left, right, top left, top right, bottom left, bottom right *)
type dir = T | B | L | R | TL | TR | BL | BR

let next_dir (x, y) = function
  | T -> (x - 1, y)
  | B -> (x + 1, y)
  | L -> (x, y - 1)
  | R -> (x, y + 1)
  | TL -> (x - 1, y - 1)
  | TR -> (x - 1, y + 1)
  | BL -> (x + 1, y - 1)
  | BR -> (x + 1, y + 1)

let input =
  In_channel.with_open_bin "inputs/04.txt" In_channel.input_lines
  |> List.map (fun s -> Array.of_seq (String.to_seq s))
  |> Array.of_list

let rows, cols = (Array.length input, Array.length input.(0))
let in_bound row col = row >= 0 && row < rows && col >= 0 && col < cols

let rec find_xmas (x, y) prev dir =
  match in_bound x y with
  | false -> 0
  | true -> (
      match (input.(x).(y), prev) with
      | 'X', None -> find_xmas (next_dir (x, y) dir) (Some 'X') dir
      | 'M', Some 'X' -> find_xmas (next_dir (x, y) dir) (Some 'M') dir
      | 'A', Some 'M' -> find_xmas (next_dir (x, y) dir) (Some 'A') dir
      | 'S', Some 'A' -> 1
      | _ -> 0)

let find_xmas_in_all_dirs (x, y) =
  let dirs = [ T; B; L; R; TL; TR; BL; BR ] in
  List.combine (List.init 8 (fun _ -> (x, y))) dirs
  |> List.fold_left (fun acc (pos, dir) -> acc + find_xmas pos None dir) 0

let part_1 =
  Array.mapi
    (fun i row -> Array.mapi (fun j _ -> find_xmas_in_all_dirs (i, j)) row)
    input
  |> Array.fold_left (fun acc row -> Array.fold_left ( + ) acc row) 0

let find_x_shaped_mas (x, y) =
  let corners = [ TL; TR; BL; BR ] in
  let rec check_corners acc = function
    | [] -> acc
    | dir :: tl ->
        let nx, ny = next_dir (x, y) dir in
        if in_bound nx ny then
          check_corners (((nx, ny), input.(nx).(ny)) :: acc) tl
        else []
  in
  match input.(x).(y) with 'A' -> check_corners [] corners | _ -> []

let is_x_mas corners =
  List.filter (fun (_, c) -> c = 'M') corners |> List.length = 2
  && List.filter (fun (_, c) -> c = 'S') corners |> List.length = 2
  &&
  match List.filter (fun (_, c) -> c = 'M') corners with
  | [ ((x, y), c); ((x', y'), c') ]
    when (c = 'M' && c' = 'M') || (c = 'S' && c' = 'S') ->
      x = x' || y = y'
  | _ -> false

let part_2 =
  Array.mapi
    (fun i row -> Array.mapi (fun j _ -> find_x_shaped_mas (i, j)) row)
    input
  |> Array.fold_left
       (fun acc row ->
         Array.fold_left
           (fun acc' col -> acc' + Bool.to_int (is_x_mas col))
           acc row)
       0

let _ =
  Printf.printf "Solution for part 1 : %d\n" part_1;
  Printf.printf "Solution for part 2 : %d\n" part_2
