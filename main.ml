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

let solve goal init moves buttons =
  (* Get all button sequences of length `moves` *)
  let all_combinations = combinations buttons moves in

  (* Filter those which evaluate to `goal` *)
  List.filter
    (fun combiation ->
       goal =
       (* Reduce the sequence of buttons *)
       List.fold_left (fun acc (_, fn) -> fn acc) init combiation)
    all_combinations

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

let () =
  print_endline "Level 11";
  solve 100 99 3 [
    ("-8",  fun n -> n - 8)  ;
    ("x11", fun n -> n * 11) ;
    ("<<",  fun n -> n / 10) ;
  ]
  |> print_solutions;

  print_newline ();
  print_endline "Level 12";
  solve 404 0 5 [
    ("+8",  fun n -> n + 8)  ;
    ("x10", fun n -> n * 10) ;
    ("/2",  fun n -> n / 2)  ;
  ]
  |> print_solutions;

  print_newline ();
  print_endline "Level 24";
  solve 210 0 5 [
    ("-5", fun n -> n - 5) ;
    ("+5", fun n -> n + 5) ;
    ("5",  fun n -> 5 + n * 10) ;
    ("2",  fun n -> 2 + n * 10) ;
  ]
  |> print_solutions;

  (* Swap `a` digits for `b`s in `n` *)
  let rec swap n a b =
    if n < 10 then
      if n = a then b else n
    else
      let last_digit = n mod 10 in
      let rest = swap (n / 10) a b in

      10 * rest +
      (if last_digit = a then b else last_digit) in

  print_newline ();
  print_endline "Level 30";
  solve 2321 0 6 [
    ("1", fun n -> 1 + n * 10) ;
    ("2", fun n -> 2 + n * 10) ;
    ("1=>2", fun n -> swap n 1 2) ;
    ("2=>3", fun n -> swap n 2 3) ;
  ]
  |> print_solutions;
