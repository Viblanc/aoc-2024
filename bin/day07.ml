let search_forward_opt rx s i =
  try Some (Str.search_forward rx s i) with Not_found -> None

let parse_line s =
  let rx = Str.regexp {|[0-9]+|} in
  let rec parse acc i =
    match search_forward_opt rx s i with
    | Some _ ->
        let n = int_of_string (Str.matched_string s) in
        parse (n :: acc) (Str.match_end ())
    | None -> acc
  in
  let ints = List.rev (parse [] 0) in
  (List.hd ints, List.tl ints)

let input =
  In_channel.with_open_bin "inputs/07.txt" In_channel.input_lines
  |> List.map parse_line

let evaluate_equation (want, l) =
  let rec compute_result got = function
    | [] -> got = want
    | n :: ns -> compute_result (got + n) ns || compute_result (got * n) ns
  in
  match l with [] -> false | hd :: tl -> compute_result hd tl

let part_1 =
  List.filter evaluate_equation input |> List.map fst |> List.fold_left ( + ) 0

let concat n k =
  let rec get_n_of_digits = function
    | 0 -> 0
    | n -> 1 + get_n_of_digits (n / 10)
  in
  let rec pow n = function 0 -> n | i -> pow (n * 10) (i - 1) in
  pow n (get_n_of_digits k) + k

let evaluate_equation (want, l) =
  let rec compute_result got = function
    | [] -> got = want
    | n :: ns ->
        compute_result (got + n) ns
        || compute_result (got * n) ns
        || compute_result (concat got n) ns
  in
  match l with [] -> false | hd :: tl -> compute_result hd tl

let part_2 =
  List.filter evaluate_equation input |> List.map fst |> List.fold_left ( + ) 0

let _ =
  Printf.printf "Solution for part 1 : %d\n" part_1;
  Printf.printf "Solution for part 2 : %d\n" part_2
