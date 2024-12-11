let parse s =
  let buf = Scanf.Scanning.from_string s in
  let rec do_parse acc =
    match Scanf.bscanf_opt buf "%d " (fun n -> n :: acc) with
    | Some l -> do_parse l
    | None -> List.rev acc
  in
  do_parse []

let stones =
  parse (In_channel.with_open_bin "inputs/11.txt" In_channel.input_all)

let split n =
  let s = string_of_int n in
  let len = String.length s in
  String.
    ( int_of_string (sub s 0 (len / 2)),
      int_of_string (sub s (len / 2) (len / 2)) )

let map = Hashtbl.create 123456

let rec count_stones n stone =
  let len = String.length (string_of_int stone) in
  let blink = function
    | 0 -> count_stones (n - 1) 1
    | s when len mod 2 = 0 ->
        let s1, s2 = split s in
        count_stones (n - 1) s1 + count_stones (n - 1) s2
    | s -> count_stones (n - 1) (2024 * s)
  in
  if n = 0 then 1
  else
    match Hashtbl.find_opt map (n, stone) with
    | None ->
        let i = blink stone in
        Hashtbl.add map (n, stone) i;
        i
    | Some i -> i

let part_1 = List.(fold_left ( + ) 0 (map (count_stones 25) stones))
let part_2 = List.(fold_left ( + ) 0 (map (count_stones 75) stones))

let _ =
  Printf.printf "Solution for part 1 : %d\n" part_1;
  Printf.printf "Solution for part 2 : %d\n" part_2
