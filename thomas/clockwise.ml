
open Reader
;;
Random.self_init ()

let neg = ref true

exception UnExpected
exception InvalidSol

exception Exit of int
let find_edge m x y =
    try
        List.iter
        (fun s ->
            let e = m.edges.(s) in
            if (e.orig = x && e.dest = y) || (e.orig = y && e.dest = x && e.twoway) then raise (Exit s))
        m.outedges.(x);
        raise InvalidSol
    with Exit i -> i

let neighbors m x =
    let l = m.outedges.(x) in
    List.fold_left (fun acc i ->
        let e = m.edges.(i) in
        if e.orig = x then
            e.dest :: acc
        else if e.dest = x && e.twoway then
            e.orig :: acc
        else
            raise UnExpected) [] l

let cross_product (x1,y1) (x2,y2) (x3,y3) =
    (x2 -. x1) *. (y3 -. y1) -. (x3 -. x1) *. (y2 -. y1)

let ccw p1 p2 p3 = !neg <> (cross_product p1 p2 p3 > 0.)

let filter_time m x l t =
    List.filter (fun y -> let i = find_edge m x y in m.edges.(i).cost <= t) l

let filter_visit m x l =
    List.filter (fun y -> let i = find_edge m x y in not m.edges.(i).used) l

let random_in_list l = List.nth l (Random.int (List.length l))

exception NoTime

let next m previous current time =
    let l1 = filter_time m current (neighbors m current) time in
    if l1 = [] then
        raise NoTime
    else begin
        let l = ref (filter_visit m current l1) in
        if !l = [] then
            try
                (* neg := not !neg; *)
                let (dist,d,pa) = Pathfind.dijkstra m current (fun dist x ->
                List.exists (fun e -> not (m.edges.(e).used) && m.edges.(e).cost <= time - dist) m.outedges.(x)) in
                List.hd (List.rev pa)
            with Not_found ->
                random_in_list l1
        else begin
            let next = ref (List.hd !l) in
            l := List.tl !l;
            while !l <> [] do
                let temp = List.hd !l in
                if ccw m.verts.(current) m.verts.(temp) m.verts.(!next) then
                    if not (ccw m.verts.(current) m.verts.(previous) m.verts.(!next) &&
                            ccw m.verts.(previous) m.verts.(current) m.verts.(temp)) then
                        next := temp;
                l := List.tl !l
            done;
            !next
        end
    end

(*
let rec next m previous current time =
    try
        let (dist,d,pa) = Pathfind.dijkstra m current (fun dist x -> x <> current &&
            List.exists (fun e -> not (m.edges.(e).used) && m.edges.(e).cost <= time - dist) m.outedges.(x)) in
        List.hd (List.rev pa)
    with Not_found ->
        raise NoTime
*)

let rec run_aux m previous current time =
    try
        let n = next m previous current time in
        let i = find_edge m current n in
        m.edges.(i).used <- true;
        current :: (run_aux m current n (time - m.edges.(i).cost))
    with NoTime -> [current]

let run m start time = run_aux m (random_in_list (neighbors m start)) start time

let solve_right pb =
    (* Random.self_init (); *)
    let l = ref [] in
    for i = 1 to pb.vehicles do
        let t = run pb pb.start pb.runtime in
        l := t :: !l
    done;
    !l

let plan_clock p = 
  let res = ref [] in 
  res := run p p.start p.runtime :: !res;
  for i = 1 to p.vehicles - 1 do 
    let waypoint = Random.int p.nverts in 
    (* Printf.eprintf "Waypoint : %d\n" waypoint; *)
    (* Optim : marquer used les chemins qu'on a emprunté pour venir *) 
    let (d,_,pa) = Pathfind.dijkstra p p.start (fun _ x -> x = waypoint) in 
(*    let pa = Array.to_list (Array.sub (Array.of_list (List.rev pa)) 0 (List.length pa / 2)) in *) 
    let pa' = p.start::(List.rev pa) in 
    let waypoint = List.hd (List.rev pa') in 
    let pa = List.rev (List.tl (List.rev pa')) in
    (*
    List.iter2 (fun i j ->
      let e = Check.find_edge p i j in
      p.edges.(e).used <- true
    ) pa (List.tl pa');
    *)
    res := (pa @ run p waypoint (p.runtime - d)) :: !res
  done; 
  !res 

