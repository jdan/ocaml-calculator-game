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
