let parse_registers lines =
  let rec aux (a, b, c) = function
    | [] -> (a, b, c)
    | s :: tl ->
        let registers =
          Scanf.sscanf s "Register %c: %d" (fun r n ->
              match r with 'A' -> (n, b, c) | 'B' -> (a, n, c) | _ -> (a, b, n))
        in
        aux registers tl
  in
  aux (0, 0, 0) lines

let parse_program s =
  let pos = String.length "Program: " in
  let numbers = String.sub s pos (String.length s - pos) in
  String.split_on_char ',' numbers |> List.map int_of_string |> Array.of_list

let registers, program =
  let arr =
    In_channel.with_open_bin "inputs/17.txt" In_channel.input_lines
    |> List.filter (fun s -> not (String.length s = 0))
    |> Array.of_list
  in
  (parse_registers [ arr.(0); arr.(1); arr.(2) ], parse_program arr.(3))

let rec exec_instruction registers output pointer =
  if pointer >= Array.length program then List.rev output
  else
    let a, b, c = registers in
    let opcode = program.(pointer) in
    let operand = program.(pointer + 1) in
    let co =
      match operand with
      | 0 | 1 | 2 | 3 -> operand
      | 4 -> a
      | 5 -> b
      | 6 -> c
      | _ -> failwith "should not happen"
    in
    match opcode with
    | 0 -> exec_instruction (a lsr co, b, c) output (pointer + 2)
    | 1 -> exec_instruction (a, b lxor operand, c) output (pointer + 2)
    | 2 -> exec_instruction (a, co mod 8, c) output (pointer + 2)
    | 3 when a = 0 -> exec_instruction registers output (pointer + 2)
    | 3 -> exec_instruction registers output operand
    | 4 -> exec_instruction (a, b lxor c, c) output (pointer + 2)
    | 5 -> exec_instruction registers ((co mod 8) :: output) (pointer + 2)
    | 6 -> exec_instruction (a, a lsr co, c) output (pointer + 2)
    | 7 -> exec_instruction (a, b, a lsr co) output (pointer + 2)
    | _ -> failwith "should not happen"

let part_1 =
  exec_instruction registers [] 0 |> List.map string_of_int |> String.concat ","

let rec find_a_value a idx =
  if exec_instruction (a, 0, 0) [] 0 = Array.to_list program then [ a ]
  else
    let bs = List.init 8 Fun.id in
    List.filter_map
      (fun b ->
        let a = (a lsl 3) lor b in
        let out = exec_instruction (a, 0, 0) [] 0 in
        match List.hd out = program.(idx) with true -> Some a | false -> None)
      bs
    |> List.concat_map (fun a -> find_a_value a (idx - 1))

let part_2 = List.hd (find_a_value 0 (Array.length program - 1))

let _ =
  Printf.printf "Solution for part 1: %s\n" part_1;
  Printf.printf "Solution for part 2: %d\n" part_2
