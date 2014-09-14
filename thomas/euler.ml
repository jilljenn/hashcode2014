open Reader

exception Done

let need_add_in p v = 
  let numin = List.length (List.map (fun e -> p.edges.(e).dup) (List.filter (fun e -> not (p.edges.(e).twoway)) p.inedges.(v))) in
  let numout = List.length (List.map (fun e -> p.edges.(e).dup) (List.filter (fun e -> not (p.edges.(e).twoway)) p.outedges.(v))) in
  let numinout = List.length (List.map (fun e -> p.edges.(e).dup) (List.filter (fun e -> p.edges.(e).twoway) p.outedges.(v))) in
  (numin + numout + numinout mod 2 <> 0) ||
  (numin +numinout < numout)


let need_add_out p v =
  let numin = List.length (List.map (fun e -> p.edges.(e).dup) (List.filter (fun e -> not (p.edges.(e).twoway)) p.inedges.(v))) in
  let numout = List.length (List.map (fun e -> p.edges.(e).dup) (List.filter (fun e -> not (p.edges.(e).twoway)) p.outedges.(v))) in
  let numinout = List.length (List.map (fun e -> p.edges.(e).dup) (List.filter (fun e -> p.edges.(e).twoway) p.outedges.(v))) in
  (numin + numout + numinout mod 2 <> 0) ||
  (numout +numinout < numin)

let edgedup p = 
  let rate i = 
    (*float_of_int (p.edges.(i).score) /.*) float_of_int (p.edges.(i).cost) in
  let waste = ref 0 in (* Not full budget *)
  (* Look at the cost-sorted edges *)
  let ees = Array.init p.nedges (fun i -> i) in
  Array.sort (fun i j -> compare (rate i) (rate j))
    ees;
  for i = 0 to p.nedges/20 do
    let e = p.edges.(ees.(i)) in
    let {orig;dest} = e in
    if need_add_out p orig && need_add_in p dest
    then (e.dup <- e.dup - 1; waste := e.cost + !waste)
    else if need_add_out p dest && need_add_in p dest && e.twoway
    then (e.dup <- e.dup -1; waste := e.cost + !waste)
    (* We only dup edges in one way ? *)
  done;
  Printf.eprintf "Wasted %d\n" !waste;
  flush stderr
  

let next p edge k =
  let e = p.edges.(edge) in
  if e.orig = k then e.dest else e.orig

module H = Heap.Make(struct type t = int * int * int let le = (<=) end)

(* First we compute the spanning tree *)
let spanning_tree p edges =
  let mark = Array.make p.nverts false in
  let tr = Array.make p.nverts [] in
  let h = H.insert (0,p.start) H.empty in
  let rec explore heap =
    let (dist, v, orig), heap = H.extract_min heap in
    if mark.(v) then heap
    else begin
      mark.(v) <- true;
      tr.(orig) <- v::tr.(orig);
      let heap = List.fold_left (fun h e ->
        let ee = p.edges.(e) in
        H.insert (ee.cost, next p e v, v) h
      ) heap edges.(v) in
      explore heap
    end
  in
  (try ignore (explore h) with Heap.Empty -> ());
  tr
(*
(* Two options : work with one or two trees ? *)
let dup_arrs p =
  let outtree = spanning_tree p p.outedges in
  let intree = spanning_tree p p.inedges in
  (* Now, the order in which the edges are considered will be important ! *)
  (* We can't do anything optimal but we'll try our best
     (what happens is that if we want to add an inedge we have two possible
      options : send it below according to the other tree, or send it
      up according to our tree) *)
  (* Return an integer : number of things that went up *)
  let rec explore from =
    
*)  


(* Choose a random vertex, run greedily *)
(* Then find shortest paths between all pairs and glue the shortest paths
   together *)
let euler p =
(*  edgedup p;*)(*
  let goodvert v = 
    List.exists (fun e -> p.edges.(e).used <> true) p.outedges.(v) in *)
  let vertscore v =
    List.length (List.filter (fun e -> p.edges.(e).dup > 0) p.outedges.(v)) in
  let fragments = ref [] in
  let cost = ref 0 in
  let rec miamiam v =
    (* On essaie d'aller jusqu'à un goodvert : les grands fragments sont
       des bons fragments *)
    match List.fold_left min (2, Obj.magic (), -1) 
      (List.map (fun e ->
        let ee = p.edges.(e) in
        let next = if ee.orig = v then ee.dest else ee.orig in
        ((if ee.dup <= 0 then 3 else -(ee.score)), ee, next)) p.outedges.(v)) with
        | (_,_,-1) -> (Obj.magic (), [v])
        | (_,ee,next) -> (cost := !cost + ee.cost; ee.dup <- ee.dup - 1; let (e,l) = miamiam next in 
                                           (ee,v::l))
  in

  for v = 0 to p.nverts - 1 do
    let bad = ref 0 in
    while vertscore v > !bad do
      let (ee,l) = miamiam v in
   (*   incr bad;
      ee.dest <- List.hd (List.rev l);
      ee.used <- false; *)
      fragments := l :: !fragments
    done
  done;
    Printf.eprintf "Cost first : %d\n" !cost;
  (* Pairing optimal des fragments ? *)
  let outfrags = Array.make p.nverts [] in
  let fragments = Array.of_list !fragments in
  Array.iteri (fun i (v::_) -> outfrags.(v) <- i::outfrags.(v) ) fragments;
  Printf.eprintf "i has %d fragments\n" (Array.length fragments);
  let rec greedy pos dist acc = 
    try 
      let (dist',next,pa) = Pathfind.dijkstra p pos (fun _ i -> List.length outfrags.(i) > 0) in
      let u = List.hd outfrags.(next) in
      let l = List.tl fragments.(u) in
      outfrags.(next) <- List.tl outfrags.(next);
      greedy (List.hd (List.rev fragments.(u))) (dist+dist') (List.rev l@pa@acc) 
    with Not_found -> (acc,dist)
  in
  let (acc,dist) = greedy p.start 0 [p.start] in
  Printf.eprintf "WASTED step 1 : %d, acc : %d\n" dist (List.length acc);
  let path = ref (List.rev acc) in
  let res = ref [] in
  for i = 0 to p.vehicles - 1 do
    let mypath = ref [p.start] in
    match !path with
      | start:: _ ->
        let (dist,_,papa) = Pathfind.dijkstra p p.start (fun _ x -> x = start) in
        Printf.eprintf "WASTED step 2 : %d\n" dist;
        mypath := papa @ !mypath;
        let rec run rt =
          match !path with
            | x::y::ys ->
              let e = Check.find_edge p x y in
              let ee = p.edges.(e) in
              if ee.cost > rt
              then (Printf.eprintf "Blocked %d %d\n" ee.cost rt)
              else
                (path := y::ys;
                 mypath := y::!mypath;
                 run (rt - ee.cost)
                )
            | _ -> (Printf.eprintf "DONE ?!\n")
        in
        run (p.runtime - dist);
        res := List.rev !mypath :: !res
      | [] -> assert false
  done;
  !res
    
      

      
