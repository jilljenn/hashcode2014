let read_int c = Scanf.fscanf c " %d" (fun x -> x)

let read n =
  let f = open_in n in
  let ncars = read_int f in
  Array.to_list (Array.init ncars (fun _ ->
    let npoints = read_int f in
    Array.to_list (Array.init npoints (fun _ -> read_int f))
  ))
