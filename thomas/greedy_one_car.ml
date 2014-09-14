open Reader

(* Returns best next node taking `steps` steps into account *)
let greedy p steps start =
  let follow edge start =
    if edge.orig = start then edge.dest
    else edge.orig (* Unsafe *)
  in
  let rec max_list by = function
  | [x] -> x
  | h :: t ->
    let m = max_list by t in
    if by h > by m then h else m
  in
  let rec explore steps score cost i cur =
    if steps = 0 then score, cost, [i, cur] else
    let l = List.map
      (fun i ->
        let used = p.edges.(i).used in
        let s = score + (if used then 0 else p.edges.(i).score) in
        p.edges.(i).used <- true;
        let e =
          explore (steps-1) s (cost+p.edges.(i).cost) i
            (follow p.edges.(i) cur)
        in
        p.edges.(i).used <- used;
        e, i) p.outedges.(cur)
    in
    let (s, c, l), i = max_list (fun ((s, c, _), _) -> float s /. float c) l in
    s, c, (i, cur)::l
  in
  let _, _, _ :: (i, ans) :: _ = explore steps 0 0 (-1) start in
  ans, p.edges.(i)

let rec full p time cur =
  let next, edge = greedy p 3 cur in
  if edge.cost > time then [] else begin
  edge.used <- true;
  next :: full p (time - edge.cost) next
end

let () =
  let p = read () in
  print_endline (string_of_int p.vehicles);
  for i = 1 to p.vehicles do
    let path = full p p.runtime p.start in
    let n = List.length path + 1 in
    print_endline (string_of_int n);
    print_endline (string_of_int p.start);
    List.iter (fun i -> print_endline (string_of_int i)) path;
  done
