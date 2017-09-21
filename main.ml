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

type button = string * (int -> int)
type state = {
  goal: int;
  init: int;
  moves: int;
  buttons: button list;
}

let solve { goal; init; moves; buttons } =
  (* Get all button sequences of length `moves` *)
  let all_combinations = combinations buttons moves in

  (* Filter those which evaluate to `goal` *)
  List.filter
    (fun combiation ->
       goal =
       (* Reduce the sequence of buttons *)
       List.fold_left (fun acc (_, fn) -> fn acc) init combiation)
    all_combinations

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

let print_solutions solns =
  List.map
    (fun solution ->
       (* Join strings with " " *)
       List.fold_left
         (fun acc (button, _) -> acc ^ " " ^ button)
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
      ("-8",  fun n -> n - 8)  ;
      ("x11", fun n -> n * 11) ;
      ("<<",  fun n -> n / 10) ;
    ]
  };

  print_newline ();
  solve_and_print "Level 12" {
    goal = 404 ;
    init = 0 ;
    moves = 5 ;
    buttons = [
      ("+8",  fun n -> n + 8)  ;
      ("x10", fun n -> n * 10) ;
      ("/2",  fun n -> n / 2)  ;
    ] ;
  };

  print_newline ();
  solve_and_print "Level 24" {
    goal = 210 ;
    init = 0 ;
    moves = 5 ;
    buttons = [
      ("-5", fun n -> n - 5) ;
      ("+5", fun n -> n + 5) ;
      ("5",  fun n -> 5 + n * 10) ;
      ("2",  fun n -> 2 + n * 10) ;
    ] ;
  };

  print_newline ();
  solve_and_print "Level 30" {
    goal = 2321 ;
    init = 0 ;
    moves = 6 ;
    buttons = [
      ("1", fun n -> 1 + n * 10) ;
      ("2", fun n -> 2 + n * 10) ;
      ("1=>2", fun n -> swap n 1 2) ;
      ("2=>3", fun n -> swap n 2 3) ;
    ] ;
  };

  print_newline ();
  solve_and_print "Level 44" {
    goal = 52 ;
    init = 44 ;
    moves = 5 ;
    buttons = [
      ("+9", (+) 9) ;
      ("/2", fun n -> n / 2) ;
      ("x4", ( * ) 4) ;
      ("+/-", ( * ) (-1)) ;
    ] ;
  };

  print_newline ();
  solve_and_print "Level 56" {
    goal = 100 ;
    init = 1101 ;
    moves = 4 ;
    buttons = [
      ("-1", fun n -> n - 1) ;
      ("Reverse", reverse) ;
    ] ;
  };

  print_newline ();
  solve_and_print "Level 70" {
    goal = 81 ;
    init = 7 ;
    moves = 5 ;
    buttons = [
      ("-9", fun n -> n - 9) ;
      ("x3", ( * ) 3) ;
      ("+4", (+) 4) ;
      ("+/-", ( * ) (-1)) ;
      ("Reverse", reverse) ;
    ] ;
  };
