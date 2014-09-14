open Reader

let geodist (x1,y1) (x2,y2) =
  let fx = 10000.0 /. 360.0 in
  let fy = fx *. 0.66 in
  sqrt(fx*.fx*.(x1-.x2)*.(x1-.x2) +. fy*.fy*.(y1-.y2)*.(y1-.y2))


(* better greedy : ajoute le score donné par greedy sur le chemin suivant *)
let greedy p =
  let close_to u v =
    let d = geodist (p.verts.(u)) (p.verts.(v)) in
    (* Printf.eprintf "Distance : %f\n" d; *)
    d <= 1.0
  in
  (* optimizes rate, returns score and dist *)
  let rec estimate depth excl k rt center =
    if depth <= 0 || rt <= 0 then (0.0,0,0,(-1))
    else
      (* If already visited, score 0 *)
      List.fold_left max ((-1.0), 0, 0, (-1))
        (List.map (fun e -> 
          let ee = p.edges.(e) in
          let next = if ee.orig = k then ee.dest else ee.orig in
          let myscore =
            if ee.used || List.mem e excl || not (close_to next center) then 0
            else ee.score
          in
          
          let (prevrate,nextscore,nextcost,x) = estimate (depth-1) (e::excl) next (rt - ee.cost) center in
          (* Totalement incorrect *)
          let score =
            if ee.cost + nextcost > rt then (-1000)
            else myscore + nextscore
          in
          let cost = ee.cost + nextcost in
          let rate = float_of_int myscore /. float_of_int ee.cost in
          (prevrate+.rate, score, cost, e)
         ) p.outedges.(k))
  in
  let estimate depth k rt center =
    let (_,score,_,edge) = estimate depth [] k rt center in
    if score <= 0 || edge = -1 then None
    else Some(edge)
  in
  (* Can we find a new point inside the cur_center ?
     If true, then go to this point !
     If not, go away and do it again *)
  let rec greedy path rt cur cur_center =
    flush stderr;
    match estimate 1 cur rt cur_center with
      | Some(e) ->
        let ee = p.edges.(e) in
        ee.used <- true;
        let next = if ee.orig = cur then ee.dest else if ee.dest = cur then ee.orig else assert false in
        greedy (next::path) (rt - p.edges.(e).cost) next cur_center
      | _ -> 
        let f = try 
          let (dist,d,pa) = Pathfind.dijkstra p cur (fun dist x ->
            dist <= 1000 && 
            List.exists (fun e -> not (p.edges.(e).used) && p.edges.(e).cost <= rt- dist) p.outedges.(x)) in
          if (List.length pa  = 0) then raise Not_found
          else if dist > rt then raise Not_found
          else (fun () -> greedy (pa@path) (rt-dist) d cur_center)
        with
          | Not_found -> (fun () -> 
            Printf.eprintf "NEW CENTER\n";
            let f = try 
                      let (dist,d,pa) = Pathfind.dijkstra p cur (fun dist x ->
                        List.exists (fun e -> not (p.edges.(e).used) && p.edges.(e).cost <= rt- dist) p.outedges.(x)) in
                      if (List.length pa  = 0) then raise Not_found
                      else if dist > rt then raise Not_found
                      else (fun () -> greedy (pa@path) (rt-dist) d d)
              with Not_found -> fun () -> path
            in
            f ())
        in
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
  let path = greedy [p.start] p.runtime p.start p.start in
  List.rev path
  (*
  Printf.printf "%d\n" (List.length path);
  List.iter (fun i -> Printf.printf "%d\n" i) (List.rev path)
  *)



let good_greedy p =
    let res = ref [] in
    for i = 1 to p.vehicles do
      Printf.eprintf "DOING CAR %d\n" i;
        res := greedy p :: !res;
      Printf.eprintf "DONE CAR %d\n" i;
    done;
    !res

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
