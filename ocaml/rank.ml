type t = (FieldModel.field, float) Hashtbl.t
let create () = Hashtbl.create ~random:false 200



let score mdl test fld =
  (try
    let {length=dlen1;occurrences=fldmdl1} =
      Hashtbl.find mdl fld in
    let {length=dlen2;occurrences=fldmdl2} =
      try  Hashtbl.find test fld
      with Not_found -> (0,0)
    let lendist = abs (dlen1 -. dlen2) in 
    let ngramdist =
      FieldModel.fold
	(fun n11 n2 -> n1 +. n2)
	(fun ngrm r2 n ->
	 let  r1 = try FieldModel.find fldmdl1 ngrm
		   with Not_found -> FieldModel.size fldmdl1
	 in  (abs (r1.mean -. r2.mean))+n
	     else l)
	fldmdl2 0.0 
    in List.concat (lendist,ngramdist)
   with Not_found -> (infinity,infinity)

let modelScore mdl ltest =
  let smdl = create () in
  begin
    
  in List.iter
       (fun test -> begin Hashtbl.iter (fun fld _ -> Hashtbl.replace smdl (score mdl test fld)) test; smdl end)
       ltest
