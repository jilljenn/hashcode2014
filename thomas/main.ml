
open Format
open Check
open Reader

let fun_of_arg () =
    let f = match Sys.argv.(1) with
    | "center" -> Centered.centered_greedy
    | "clock" -> Clockwise.solve_right
    | "plan_clock" -> Clockwise.plan_clock
    | _ -> Greedy.plan_greedy
    in
    if Array.length Sys.argv = 3 then
        match Sys.argv.(2) with
        | "iter" -> iter_solve f
        | _ -> wrap_solve f
    else
        wrap_solve f


let () =
  if Array.length Sys.argv = 1 then
    (* To be deprecated *)
    let prob = read () in
    wrap_solve Greedy.plan_greedy prob
  else match Sys.argv.(1) with
  | "parse" ->
    let prob = read () in
    let sol = Parse.read (Sys.argv.(2)) in
    if check_solution prob sol then begin
	let r, actual = solution_score prob sol in
	let max = max_score prob in
	Format.eprintf "%d / %d (%d to go) wasted : %d@." actual max (max - actual) r;
	print_to_file prob sol
    end else begin
	Format.eprintf "WRONG !@.";
	exit 1
    end;
    Format.eprintf "OK@.";
  | _ ->
    let f = fun_of_arg () in
    let prob = read () in
    f prob

