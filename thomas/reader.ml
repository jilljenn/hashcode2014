let read_int () = Scanf.scanf " %d" (fun x -> x)

exception ReadError

type edge = {
  orig : int;
  dest : int;
  twoway : bool;
  cost : int;
  score : int;
  mutable used : bool
}

type problem = {
  nverts : int;
  nedges : int;
  runtime : int;
  vehicles : int;
  start : int;
  verts : (float * float) array;
  edges : edge array;
  inedges : int list array;
  outedges : int list array
}

let read () =
  let nverts = read_int () in
  let nedges = read_int () in
  let runtime = read_int () in
  let vehicles = read_int () in
  let start = read_int () in
  let read_vert _ = Scanf.scanf " %f %f\n" (fun x y -> (x,y)) in
  let verts = Array.init nverts read_vert in
  let read_edge _ = Scanf.scanf " %d %d %d %d %d" (fun orig dest twoway cost score -> let twoway = match twoway with 1 -> false | 2 -> true | _ -> raise ReadError in { orig; dest; twoway; cost; score; used=false } ) in
  let edges = Array.init nedges read_edge in
  let inedges = Array.make nverts [] in
  let outedges = Array.make nverts [] in
  Array.iteri (fun i {orig; dest; twoway; _} ->
    outedges.(orig) <- i::outedges.(orig);
    inedges.(dest) <- i::inedges.(dest);
    if twoway then begin
      outedges.(dest) <- i::outedges.(dest);
      inedges.(orig) <- i::inedges.(orig)
    end
  ) edges;
  { nverts; nedges; runtime; vehicles; start; verts; edges; inedges; outedges }

(*
let _ = read ()
*)
