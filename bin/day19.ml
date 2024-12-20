let list_to_tuple = function [ a; b ] -> (a, b) | _ -> failwith "not a tuple"

let towels, designs =
  let t, d =
    In_channel.with_open_bin "inputs/19.txt" In_channel.input_all
    |> Str.(split (regexp "\n\n"))
    |> list_to_tuple
  in
  ( Str.(split (regexp ", ")) t,
    String.split_on_char '\n' d |> List.filter (fun s -> s <> "") )

let remove prefix s =
  let plen = String.length prefix in
  String.sub s plen (String.length s - plen)

let cache = Hashtbl.create 65536

let rec is_possible design =
  if String.length design = 0 then true
  else
    let prefixes =
      List.filter (fun t -> String.starts_with ~prefix:t design) towels
    in
    match Hashtbl.find_opt cache design with
    | Some res -> res
    | None ->
        let res =
          List.exists (fun pre -> is_possible (remove pre design)) prefixes
        in
        Hashtbl.add cache design res;
        res

let part_1 = List.(length (filter is_possible designs))
let mem = Hashtbl.create 65536

let rec count_combinations design =
  if String.length design = 0 then 1
  else
    let prefixes =
      List.filter (fun t -> String.starts_with ~prefix:t design) towels
    in
    match Hashtbl.find_opt mem design with
    | Some n -> n
    | None ->
        let res =
          List.fold_left
            (fun acc pre -> acc + count_combinations (remove pre design))
            0 prefixes
        in
        Hashtbl.add mem design res;
        res

let part_2 = List.(fold_left ( + ) 0 (map count_combinations designs))

let _ =
  Printf.printf "Solution for part 1: %d\n" part_1;
  Printf.printf "Solution for part 2: %d\n" part_2
