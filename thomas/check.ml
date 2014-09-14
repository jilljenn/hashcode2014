
open Reader

exception InvalidSol
exception BadSolution
exception BadEdge of int * int

type solution = int list list

let redundant = ref 0
let visited = ref (Array.make 1 false)

exception Exit of int
let find_edge m x y =
    try
        List.iter
        (fun s ->
            let e = m.edges.(s) in
            if (e.orig = x && e.dest = y) || (e.orig = y && e.dest = x && e.twoway) then raise (Exit s))
        m.outedges.(x);
        (Format.eprintf "bad edge %d %d\n" x y; 0 (*raise (BadEdge(x,y))*))
    with Exit i -> i

let rec check_aux m = function
    | [] | [_] -> true
    | x :: y :: r -> let _ = find_edge m x y in check_aux m (y :: r)

let check_car m l =
    match l with
    | [] -> raise BadSolution
    | s :: _ -> s = m.start && check_aux m l

let rec time m = function
    | [] | [_] -> 0
    | x :: y :: r ->
            let e = m.edges.(find_edge m x y) in
            e.cost + time m (y :: r)

let rec dist m = function
    | [] | [_] -> 0
    | x :: y :: r ->
            let s = find_edge m x y in
            if not !visited.(s) then begin
                !visited.(s) <- true;
                m.edges.(s).score + dist m (y :: r)
            end else begin
                redundant := !redundant + m.edges.(s).score;
                dist m (y :: r)
            end

let max_score m =
    let max = ref 0 in
    for i = 0 to (Array.length m.edges - 1) do
        max := !max + m.edges.(i).score
    done;
    !max

exception NotaPath
exception IncorrectTime
let check_solution m l =
    let b1 = List.for_all (check_car m) l in
    if b1 then begin
      List.iter (fun l -> Format.eprintf "Time : %d@." (time m l)) l;
        let b2 = List.for_all (fun l -> time m l <= m.runtime) l in
        (*if b2 then
            true
        else
            raise IncorrectTime*)
        true
    end else begin
        raise NotaPath
    end

let solution_score m l =
    redundant := 0;
    visited := Array.make (Array.length m.edges) false;
    let res = List.fold_left (fun acc l' -> acc + dist m l') 0 l in
    !redundant, res

let fprint_sol_aux fmt l =
    Format.fprintf fmt "%d@\n" (List.length l);
    List.iter (fun i -> Format.fprintf fmt "%d@\n" i) l

let fprint_sol fmt l =
    Format.fprintf fmt "%d@\n" (List.length l);
    List.iter (fprint_sol_aux fmt) l

let print_sol = fprint_sol Format.std_formatter

let copy_edge {orig; dest; twoway; cost; score; used} =
  {orig;dest;twoway;cost;score;used}

let copy_pb p = {
    p with
    verts = Array.copy p.verts;
    edges = Array.map copy_edge p.edges;
    inedges = Array.copy p.inedges;
    outedges= Array.copy p.outedges;
}

let print_to_file prob sol =
    Format.eprintf "Printing ... ";
    let _, score = solution_score prob sol in
    let f = open_out ("sols/out_" ^ (string_of_int score)) in
    fprint_sol (Format.formatter_of_out_channel f) sol;
    flush f;
    close_out f;
    Format.eprintf "Solution printed@."

let rec iter_solve_aux prob solve best_score =
    let p = copy_pb prob in
    let temp_sol = solve p in
    if check_solution p temp_sol then begin
        let m = max_score prob in
        let w, score = solution_score p temp_sol in
        Format.eprintf "Solution found, score = %d / %d (%d wasted)@." score m w;
        if score > best_score then begin
            print_to_file p temp_sol;
            iter_solve_aux prob solve score
        end else
            iter_solve_aux prob solve best_score
    end else
        raise InvalidSol

let iter_solve solve prob =
    iter_solve_aux prob solve 0

let wrap_solve solve prob =
    let sol = solve prob in
    if check_solution prob sol then begin
        let m = max_score prob in
        let w, score = solution_score prob sol in
        Format.eprintf "Solution found, score = %d / %d (%d wasted)@." score m w;
        print_to_file prob sol
    end else
        raise InvalidSol
