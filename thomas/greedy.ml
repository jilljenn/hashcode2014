Random.self_init ();;

open Reader

let rd =
  let x = ref true in
  fun () -> x := not !x; if !x then 2 else 1

(* better greedy : ajoute le score donné par greedy sur le chemin suivant *)
let greedy p =
  (* optimizes rate, returns score and dist *)
  let rec estimate depth excl k rt =
    if depth <= 0 || rt <= 0 then (0.0,0,0,(-1))
    else
      (* If already visited, score 0 *)
      List.fold_left max ((-1.0), 0, 0, (-1))
        (List.map (fun e -> 
          let ee = p.edges.(e) in
          let myscore =
            if ee.used || List.mem e excl then 0
            else ee.score
          in
          let next = if ee.orig = k then ee.dest else ee.orig in
          let (prevrate,nextscore,nextcost,x) = estimate (depth-1) (e::excl) next (rt - ee.cost) in
          (* TOtalement incorrect *)
          let score =
            if ee.cost + nextcost > rt then (-1000)
            else myscore + nextscore
          in
          let cost = ee.cost + nextcost in
          let rate = float_of_int myscore /. float_of_int ee.cost in
          (prevrate+.rate, score, cost, e)
         ) p.outedges.(k))
  in
  let estimate depth k rt =
(*    Printf.eprintf "EST START\n"; *)
    let (_,score,_,edge) = estimate depth [] k rt in
(*    Printf.eprintf "EST END\n"; *)
    if score <= 0 || edge = -1 then None
    else Some(edge)
  in
  let rec greedy path rt cur =
    (* Take the best unvisited street *)
    (*let edges = List.map (fun e ->
      let ee = p.edges.(e) in
      let rate = 
        if ee.used then 0.0
        else if ee.cost > rt then (-1.0)
        else float_of_int ee.score /. float_of_int ee.cost in
      (-. rate,e)
    ) p.outedges.(cur) in
    let edges = List.sort compare edges in *)
 (*   Printf.eprintf "rt = %d\n" rt; *)
    match estimate 1 cur rt with
      | Some(e) ->
        let ee = p.edges.(e) in
        ee.used <- true;
        let next = if ee.orig = cur then ee.dest else if ee.dest = cur then ee.orig else assert false in
        greedy (next::path) (rt - p.edges.(e).cost) next
      | _ -> 
        let f = try 
          let (dist,d,pa) = Pathfind.dijkstra p cur (fun dist x ->
            List.exists (fun e -> not (p.edges.(e).used) && p.edges.(e).cost <= rt- dist) p.outedges.(x)) in
          (* Printf.eprintf "Jumped %d\n" dist; *)
          if (List.length pa  = 0) then fun () -> path
          else if dist > rt then (fun () -> path)
          else (fun () -> greedy (pa@path) (rt-dist) d)
        with
          | Not_found -> fun () -> path in
        f ()
(*      | (score,e)::_ when score = 0.0 ->
        begin try let nexts = List.filter (fun e ->
          let ee = p.edges.(e) in
          ee.cost <= rt) p.outedges.(cur) in
        Some(List.nth nexts (Random.int (List.length nexts)))
        with _ -> None end
      | _ -> None *)
(*    match next with
      | None -> path
      | Some(e) ->
        let ee = p.edges.(e) in
        ee.used <- true;
        let next = if ee.orig = cur then ee.dest else ee.orig in
        greedy (next::path) (rt - p.edges.(e).cost) next*)
  in
  let path = greedy [p.start] p.runtime p.start in
  List.rev path
  (*
  Printf.printf "%d\n" (List.length path);
  List.iter (fun i -> Printf.printf "%d\n" i) (List.rev path)
  *)

let good_greedy p =
    let res = ref [] in
    for i = 1 to p.vehicles do
        res := greedy p :: !res;
      Printf.eprintf "DONE CAR %d\n" i;
    done;
    !res

let opti (c1, c2) p =
  snd (Array.fold_left max (-1000000.0,4516)
    (Array.mapi (fun i (x,y) ->
      (c1*.x+.c2*.y,i)) p.verts))
  (* Find the vertex with the best c1*x + c2 * y *)
  

let starts = [|(-1.0,-1.0);(-1.0,0.0);(-1.0,1.0);(0.0,1.0);(1.0,1.0);
              (1.0,0.0);(1.0,-1.0);(0.0,-1.0)|]

let plan_greedy p =
  let res = ref [] in
  for i = 1 to p.vehicles do
    let waypoint = Random.int p.nverts in
    Printf.eprintf "Waypoint : %d\n" waypoint;
    (* Optim : marquer used les chemins qu'on a emprunté pour venir *)
    let (d,_,pa) = Pathfind.dijkstra p p.start (fun _ x -> x = waypoint) in
(*    let pa = Array.to_list (Array.sub (Array.of_list (List.rev pa)) 0 (List.length pa / 2)) in *)
    let pa' = p.start::(List.rev pa) in
    let waypoint = List.hd (List.rev pa') in
    let pa = List.rev (List.tl (List.rev pa')) in
    List.iter2 (fun i j ->
      let e = Check.find_edge p i j in
      p.edges.(e).used <- true
    ) pa (List.tl pa'); 
    res := (pa @ greedy {p with start = waypoint; runtime = p.runtime - d}) :: !res
  done;
  !res
