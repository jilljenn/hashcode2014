(* Just a dumb pathfinding *)

open Reader

module H = Heap.Make(struct type t = int * int * int let le = (<=) end)

let infinity = 1_000_000_000

let dijkstra p src destcond = 
  try
  let dist = Array.make p.nverts infinity in
  let previous = Array.make p.nverts (-1) in
  let heap = H.insert (0,src,0) H.empty in
  (* dist.(src) <- 0; *)
  let rec explore heap =
    let ((d,x,prev),heap) = H.extract_min heap in
(*    Printf.eprintf "Extracted %d %d %d\n" d x prev; *)
    if dist.(x) <> infinity
    then explore heap
    else 
      if destcond d x then (previous.(x) <- prev; (d,x))
      else begin
        dist.(x) <- d;
        previous.(x) <- prev;
        explore (List.fold_left (fun h e ->
          let ee = p.edges.(e) in
          let next = if ee.orig = x then ee.dest else ee.orig in
          H.insert (d+ee.cost, next, x) h
        ) heap p.outedges.(x))
      end
  in
  let (d,dest) = explore heap in
  let rec find_path d =
(*    Printf.eprintf "At %d\n" d; *)
    if d = src then []
    else d::find_path previous.(d)
  in
  (d,dest,find_path dest)
  with Heap.Empty -> ( (* Printf.eprintf "EMPTY HEAP :(\n"; *) raise Not_found)
  
