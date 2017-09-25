(* Not really combinations, since we repeat *)
let rec combinations options = function
  | 0 -> [[]]
  | count ->
    List.map
      (fun choice ->
         let rest = combinations options (count - 1) in
         List.map (fun item -> choice :: item) rest)
      options
    |> List.flatten

type button =
  | Add of int
  | Subtract of int
  | Multiply of int
  | Divide of int
  | Append of int
  | PlusMinus
  | Swap of int * int
  | ShiftLeft
  | Reverse
  | Sum
  | Mirror

type state = {
  goal: int;
  init: int;
  moves: int;
  buttons: button list;
}

type value =
  | Value of int
  | Error

let string_of_button = function
  | Add n -> "+" ^ (string_of_int n)
  | Subtract n -> "-" ^ (string_of_int n)
  | Multiply n -> "x" ^ (string_of_int n)
  | Divide n -> "/" ^ (string_of_int n)
  | Append n -> string_of_int n
  | PlusMinus -> "+/-"
  | Swap (a, b) -> (string_of_int a) ^ "=>" ^ (string_of_int b)
  | ShiftLeft -> "<<"
  | Reverse -> "Reverse"
  | Sum -> "SUM"
  | Mirror -> "Mirror"

(* Swap `a` digits for `b`s in `n` *)
let rec swap n a b =
  if abs n < 10 then
    if n = a then b else n
  else
    let last_digit = n mod 10 in
    let rest = swap (n / 10) a b in

    10 * rest +
    (if last_digit = a then b else last_digit)

let reverse n =
  let rec inner n acc =
    if abs n < 10 then 10 * acc + n
    else
      inner (n / 10) (10 * acc + (n mod 10))
  in
  inner n 0

let rec sum n =
  if abs n < 10 then n
  else (n mod 10) + sum (n / 10)

let radix n =
  1 + (n |> abs |> float_of_int |> log10 |> int_of_float)

let mirror v =
  let pow_int a b = (float_of_int a) ** (float_of_int b) |> int_of_float in
  let rev = reverse v in
  v * (pow_int 10 (radix rev)) + rev

let press_button v = function
  | Add n -> v + n
  | Subtract n -> v - n
  | Multiply n -> v * n
  | Divide n -> v / n
  | Append n -> v * 10 + n
  | PlusMinus -> v * (-1)
  | Swap (a, b) -> swap v a b
  | ShiftLeft -> v / 10
  | Reverse -> reverse v
  | Sum -> sum v
  | Mirror -> mirror v

let solve { goal; init; moves; buttons } =
  (* Get all button sequences of length `moves` *)
  let all_combinations = combinations buttons moves in

  let value_of_result button prev_value value =
    (* Can only store 6 digits in the calculator *)
    if radix value > 6
    then Error
    else
      match button with
      (* Can't swap numbers that aren't there *)
      | Swap _ ->
        if prev_value = value
        then Error
        else Value value
      (* Can only divide numbers evenly *)
      | Divide n ->
        if 0 != prev_value mod n
        then Error
        else Value value
      | _ -> Value value in

  let value_after_button_press acc button =
    match acc with
    | Error -> Error
    | Value v -> press_button v button
                 |> value_of_result button v in

  (* Filter those which evaluate to `goal` *)
  List.filter
    (fun combination ->
       (* Reduce the sequence of buttons *)
       let result = List.fold_left
           value_after_button_press
           (Value init)
           combination in

       match result with
       | Error -> false
       | Value v -> goal == v)
    all_combinations

let print_solutions solns =
  List.map
    (fun solution ->
       (* Join strings with " " *)
       List.fold_left
         (fun acc button -> acc ^ " " ^ (string_of_button button))
         ""
         solution)
    solns
  |> List.iter print_endline

let solve_and_print title state =
  print_endline title;
  solve state |> print_solutions

let () =
  solve_and_print "Level 11" {
    goal = 100 ;
    init = 99 ;
    moves = 3 ;
    buttons = [
      Subtract 8 ;
      Multiply 11 ;
      ShiftLeft ;
    ]
  };

  print_newline ();
  solve_and_print "Level 12" {
    goal = 404 ;
    init = 0 ;
    moves = 5 ;
    buttons = [
      Add 8 ;
      Multiply 10 ;
      Divide 2;
    ] ;
  };

  print_newline ();
  solve_and_print "Level 24" {
    goal = 210 ;
    init = 0 ;
    moves = 5 ;
    buttons = [
      Subtract 5 ;
      Add 5 ;
      Append 5 ;
      Append 2 ;
    ] ;
  };

  print_newline ();
  solve_and_print "Level 30" {
    goal = 2321 ;
    init = 0 ;
    moves = 6 ;
    buttons = [
      Append 1 ;
      Append 2 ;
      Swap (1, 2) ;
      Swap (2, 3) ;
    ] ;
  };

  print_newline ();
  solve_and_print "Level 44" {
    goal = 52 ;
    init = 44 ;
    moves = 5 ;
    buttons = [
      Add 9 ;
      Divide 2 ;
      Multiply 4 ;
      PlusMinus ;
    ] ;
  };

  print_newline ();
  solve_and_print "Level 56" {
    goal = 100 ;
    init = 1101 ;
    moves = 4 ;
    buttons = [
      Subtract 1 ;
      Reverse ;
    ] ;
  };

  print_newline ();
  solve_and_print "Level 70" {
    goal = 81 ;
    init = 7 ;
    moves = 5 ;
    buttons = [
      Subtract 9 ;
      Multiply 3 ;
      Add 4 ;
      PlusMinus ;
      Reverse ;
    ] ;
  };

  print_newline ();
  solve_and_print "Level 72" {
    goal = 28 ;
    init = 0 ;
    moves = 7 ;
    buttons = [
      Add 6 ;
      Subtract 3 ;
      Reverse ;
      ShiftLeft ;
    ] ;
  };

  print_newline ();
  solve_and_print "Level 124" {
    goal = 20 ;
    init = 125 ;
    moves = 7 ;
    buttons = [
      Swap (6, 2) ;
      Append 0 ;
      Mirror ;
      Sum ;
    ] ;
  };

  print_newline ();
  solve_and_print "Level 129" {
    goal = 18 ;
    init = 140 ;
    moves = 6 ;
    buttons = [
      Subtract 3 ;
      Add 9 ;
      Divide 12 ;
      Mirror ;
      ShiftLeft ;
    ] ;
  };
