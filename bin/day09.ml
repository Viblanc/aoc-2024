type block = File of int * int | Free of int

let parse blocks =
  let rec parse_free_block idx = function
    | [] -> []
    | n :: blocks -> Free n :: parse_file_block idx blocks
  and parse_file_block idx = function
    | [] -> []
    | n :: blocks -> File (idx, n) :: parse_free_block (idx + 1) blocks
  in
  parse_file_block 0 blocks

let blocks =
  In_channel.with_open_bin "inputs/09.txt" In_channel.input_line
  |> Option.get |> String.to_seq
  |> Seq.map (fun c -> int_of_char c - int_of_char '0')
  |> List.of_seq |> parse

let rec expand = function
  | [] -> []
  | File (id, n) :: tl -> List.init n (fun _ -> id) @ expand tl
  | Free n :: tl -> List.init n (fun _ -> -1) @ expand tl

let compact blocks =
  let len =
    List.fold_left (fun sum -> function -1 -> sum | _ -> sum + 1) 0 blocks
  in
  let rec move_blocks acc cur_len left right =
    if cur_len = len then List.rev acc
    else
      match (left, right) with
      | [], _ | _, [] -> failwith "should not happen"
      | _, -1 :: right -> move_blocks acc cur_len left right
      | -1 :: left, n :: right ->
          move_blocks (n :: acc) (cur_len + 1) left right
      | n :: left, _ -> move_blocks (n :: acc) (cur_len + 1) left right
  in
  move_blocks [] 0 blocks (List.rev blocks)

let part_1 =
  compact (expand blocks) |> List.mapi ( * ) |> List.fold_left ( + ) 0

let compact blocks =
  let rec move_block acc id n = function
    | [] -> failwith "should not happen"
    | Free n' :: bs when n' >= n ->
        List.rev_append
          (Free (n' - n) :: File (id, n) :: acc)
          (List.map
             (function File (id', _) when id = id' -> Free n | b -> b)
             bs)
    | File (id', _) :: bs when id = id' ->
        List.rev_append acc (File (id, n) :: bs)
    | hd :: bs -> move_block (hd :: acc) id n bs
  in
  List.fold_left
    (fun acc -> function
      | File (id, n) -> move_block [] id n acc
      | Free _ -> acc)
    blocks (List.rev blocks)

let part_2 =
  compact blocks |> expand
  |> List.mapi (fun i -> function -1 -> 0 | n -> n * i)
  |> List.fold_left ( + ) 0

let _ =
  Printf.printf "Solution for part 1 : %d\n" part_1;
  Printf.printf "Solution for part 2 : %d\n" part_2
