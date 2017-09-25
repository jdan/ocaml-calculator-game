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

type state = {
  goal: int;
  init: int;
  moves: int;
  buttons: button list;
}

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

(* Swap `a` digits for `b`s in `n` *)
let rec swap n a b =
  if n < 10 then
    if n = a then b else n
  else
    let last_digit = n mod 10 in
    let rest = swap (n / 10) a b in

    10 * rest +
    (if last_digit = a then b else last_digit)

let reverse n =
  let rec inner n acc =
    if n < 10 then 10 * acc + n
    else
      inner (n / 10) (10 * acc + (n mod 10))
  in
  inner n 0

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

let solve { goal; init; moves; buttons } =
  (* Get all button sequences of length `moves` *)
  let all_combinations = combinations buttons moves in

  (* Filter those which evaluate to `goal` *)
  List.filter
    (fun combiation ->
       goal =
       (* Reduce the sequence of buttons *)
       List.fold_left (fun acc button -> press_button acc button) init combiation)
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
  solve_and_print "Level 84" {
    goal = 196 ;
    init = 0 ;
    moves = 8 ;
    buttons = [
      Append 1 ;
      Add 12 ;
      Multiply 13 ;
      Reverse ;
      ShiftLeft ;
    ] ;
  };
