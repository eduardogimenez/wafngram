let map f tbl =
  let ntbl = Hashtbl.create ~random:false (Hashtbl.length tbl)
  in
  begin
    Hashtbl.iter (fun k e -> Hashtbl.add ntbl k (f k e)) tbl;
    ntbl
  end

let toList tbl =
  Hashtbl.fold (fun k e l -> (k,e)::l) tbl []

	       (*
exception Exists of ('a,'b)
let exists p tbl =
  Hashtbl.iter (fun k v -> if p k v then raise Exists (k,v) else ()) tbl;
  false
		*)
