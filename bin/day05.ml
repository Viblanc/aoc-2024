let list_to_tuple = function [ a; b ] -> (a, b) | _ -> failwith "not a tuple"

let parse_updates raw_updates =
  let parse_update s = String.split_on_char ',' s |> List.map int_of_string in
  List.map parse_update raw_updates

module Rules = Map.Make (Int)

let parse_rules raw_rules =
  let parse_rule s =
    String.split_on_char '|' s |> List.map int_of_string |> list_to_tuple
  in
  let to_map rules =
    let add_rule b = function None -> Some [ b ] | Some l -> Some (b :: l) in
    List.fold_left
      (fun map (a, b) -> Rules.update a (add_rule b) map)
      Rules.empty rules
  in
  List.map parse_rule raw_rules |> to_map

let input =
  let raw_rules, raw_updates =
    In_channel.with_open_bin "inputs/05.txt" In_channel.input_all
    |> Str.(split (regexp "\n\n"))
    |> List.map (fun s ->
           String.split_on_char '\n' s
           |> List.filter (fun s' -> String.empty <> s'))
    |> list_to_tuple
  in
  (parse_rules raw_rules, parse_updates raw_updates)

let rec is_in_order map list =
  match list with
  | [] | [ _ ] -> true
  | hd :: tl -> (
      match Rules.find_opt hd map with
      | None -> false
      | Some l ->
          if List.for_all (fun n -> List.mem n l) tl then is_in_order map tl
          else false)

let part_1 =
  let rules, updates = input in
  List.filter (is_in_order rules) updates
  |> List.fold_left (fun acc l -> acc + List.nth l (List.length l / 2)) 0

let sort_updates map updates =
  let rec do_sort acc queue =
    match Queue.take_opt queue with
    | None -> acc
    | Some n -> (
        match Rules.find_opt n map with
        | None ->
            if Queue.is_empty queue then n :: acc
            else (
              Queue.push n queue;
              do_sort acc queue)
        | Some l ->
            if Seq.for_all (fun n -> List.mem n l) (Queue.to_seq queue) then
              do_sort (n :: acc) queue
            else (
              Queue.push n queue;
              do_sort acc queue))
  in
  let queue = Queue.of_seq (List.to_seq updates) in
  List.rev (do_sort [] queue)

let part_2 =
  let rules, updates = input in
  List.filter (Fun.negate (is_in_order rules)) updates
  |> List.map (sort_updates rules)
  |> List.fold_left (fun acc l -> acc + List.nth l (List.length l / 2)) 0

let _ =
  Printf.printf "Solution for part 1 : %d\n" part_1;
  Printf.printf "Solution for part 2 : %d\n" part_2
