open Arff
open Stringlib


module FieldNames = struct

  (* Two tables, with the same pairs, and therefore same length. No repeated entries. *)
  type t = {
    indexes : (int,string) Hashtbl.t;
    names   : (string,int) Hashtbl.t
  }
		      
  let (self:t) = {
    indexes = Hashtbl.create ~random:false 200;
    names   = Hashtbl.create ~random:false 200;
  }

  let attributes = 
      [ "Identifier"
      ; "Class"
      (* Header *)
      ; "Method"
      ; "Protocol"
      ; "Uri" 
      ; "Query"
      ;  "Body"
	(* Standard *)
      ; "Accept" 	
      ; "Accept-Charset"
      ; "Accept-Encoding"
      ; "Accept-Language"
      ; "Accept-Datetime"
      ; "Authorization"
      ; "Cache-Control"
      ; "Connection"
      ; "Cookie"
      ; "Content-Length"
      ; "Content-MD5"
      ; "Content-Type"
      ; "Date"
      ; "Expect"
      ; "Forwarded"
      ; "From"
      ; "Host"
      ; "If-Match"
      ; "If-Modified-Since"
      ; "If-None-Match"
      ; "If-Range"
      ; "If-Unmodified-Since"
      ; "Max-Forwards"
      ; "Origin"
      ; "Pragma"
      ; "Proxy-Authorization"
      ; "Range"
      ; "Referer"
      ; "TE"
      ; "User-Agent"
      ; "Upgrade"
      ; "Via"
      ; "Warning"
      (* No standard *)
      ; "X-Requested-With"
      ; "DNT"
      ; "X-Forwarded-For"
      ; "X-Forwarded-Host"
      ; "X-Forwarded-Proto"
      ; "Front-End-Https"
      ; "X-Http-Method-Override"
      ; "X-ATT-DeviceId"
      ; "X-Wap-Profile"
      ; "Proxy-Connection"
      ; "X-UIDH"
      ; "X-Csrf-Token"
      ; "X-Request-ID"
      ; "X-Correlation-ID"
      (* Also found *)
      ; "X-FB-HTTP-Engine"
      ; "X-FB-ABSURL-DEBUG"
      ; "X-Purpose"
      ; "Keep-Alive"
      ; "DAV"
      ; "X-Moz"
      ; "UA-CPU"
      ; "Save-Data"
      ; "Purpose"
      ; "x-fb-sim-operator"
      ; "x-fb-sim-hni"
      ; "cache-control"
      ; "x-fb-net-hni"
      ; "X-IWS-Via"
      ; "Depth"
      ; "Translate"
      ; "X-FB-SIM-HNI"
      ; "X-FB-Connection-Type"
      ; "fetchergroup"
      ; "Dapper-Host-Ip"
      ; "x-wap-profile"
      ; "X-P2P-PeerDist"
      ; "X-P2P-PeerDistEx"
      ; "X-Geo"
      ; "Chrome-Proxy"
      ; "Chrome-Proxy-Accept-Transform"
      (* Response Field Names - Standard *)
      ; "Access-Control-Allow-Origin"
      ; "Accept-Patch"
      ; "Accept-Ranges"
      ; "Age"
      ; "Allow"
      ; "Alt-Svc"
      (* ; "Cache-Control" - Repeated *)
      (* ; "Connection" - Repeated *)
      ; "Content-Disposition"
      ; "Content-Encoding"
      ; "Content-Language"
      (* ; "Content-Length" - Repeated *)
      ; "Content-Location"
      (* ; "Content-MD5" - Repeated *)
      ; "Content-Range"
      (* ; "Content-Type" - Repeated *)
      (* ; "Date" - Repeated *)
      ; "ETag"
      ; "Expires"
      ; "Last-Modified"
      ; "Link"
      ; "Location"
      ; "P3P"
      (* ; "Pragma" - Repeated *)
      ; "Proxy-Authenticate"
      ; "Public-Key-Pins"
      ; "Refresh"
      ; "Retry-After"
      ; "Server"
      ; "Set-Cookie"
      ; "Status"
      ; "Strict-Transport-Security"
      ; "Trailer"
      ; "Transfer-Encoding"
      ; "TSV"
      (* ; "Upgrade" - Repeated *)
      ; "Vary"
      (* ; "Via" - Repeated *)
      (*; "Warning" - Repeated *)
      ; "WWW-Authenticate"
      ; "X-Frame-Options"
      (* Response Field Names - Non standard *)
      ; "X-XSS-Protection"
      ; "Content-Security-Policy"
      ; "X-Content-Type-Options"
      ; "X-Powered-By"
      ; "X-UA-Compatible"
      ; "X-Content-Duration"
      ; "Upgrade-Insecure-Requests"
      (* ; "X-Request-ID" - Repeated *)
      (* ; "X-Correlation-ID" - Repeated *)
      ]

	   
  let load =
    List.iteri
      (fun i x -> begin Hashtbl.add self.indexes i x; Hashtbl.add self.names x i end)
      attributes
  let classField = Listlib.index "Class" attributes
  let idField    = Listlib.index "Identifier" attributes
  let mapClassValue str =
    match str with
    | "Valid" -> "Normal"
    | "Attack" -> "Abnormal"
    | str -> str
  let stringType ()  =
    let l = Hashtbl.fold (fun i fld l -> (i,fld)::l) self.indexes [] in
    let sortedl =  List.sort (fun (i,_) (j,_) -> compare i j) l
    in List.map (fun (i,fld) -> (fld,StringType)) sortedl
  let number ()   = Hashtbl.length self.indexes
  let add fld =
    if Hashtbl.mem self.names fld
    then ()
    else
      let ord = Hashtbl.length self.indexes in
      begin
	Hashtbl.add self.indexes ord fld;
	Hashtbl.add self.names fld ord
      end
  let name  ord   = try Hashtbl.find self.indexes ord with Not_found -> raise (Invalid_argument ("Unknown field ordinal: "^(string_of_int ord)))
  let index fld   = try Hashtbl.find self.names   fld with Not_found -> raise (Invalid_argument ("Unknown field name: "^fld))
end
		      
let methods   = ["GET";"POST";"PUT";"HEAD";"DELETE";"TRACE";"CONNECT";"OPTIONS";"PROPFIND"]
let protocols = ["HTTP/1.1";"HTTP/1.0"]

let decode contents =
  if !Arff.uudecoded
  then contents
  else
    try (Netencoding.Url.decode contents)
    with _ ->
      begin
	Printf.fprintf stdout "Failed to decode field contents: %s\n" contents; flush stdout;
	contents
      end

type field = {
  (* El ordinal del campo en el HTTP request *)
  position : int;
  (* Eventualmente el nombre del registro en el formulario dentro de un campo. *)
  subfield : string option
}
let getSubfield fld =
  match fld with
  | None -> ""
  | Some s -> s
let fieldName fld =
  let stringpos fld = (if 0 <= fld.position && fld.position < 10 then "0" else "")^(string_of_int fld.position)
  in  String.concat "-" [stringpos fld;FieldNames.name fld.position;getSubfield fld.subfield]
let printField out fld =
  Printf.fprintf out "%s" (fieldName fld)
let fieldOfString n str =
  match str with
  | "None" -> {position=n;subfield = None}
  | str    -> {position=n;subfield = Some str}
      
let makeArffHeader name = {
  relname=name;
  attributes=
    ("Identifier", IntegerType)::
     ("Class",EnumeratedType ["Normal";"Abnormal"])::
       ("Method",EnumeratedType methods)::
	 ("Protocol",EnumeratedType protocols)::
	   (List.tl (List.tl (List.tl (List.tl (FieldNames.stringType ())))));
     constants=
       let tbl = Hashtbl.create 4 in
       begin
	 List.iter (fun x -> Hashtbl.add tbl x "Protocol") protocols;
	 List.iter (fun x -> Hashtbl.add tbl x "Method") methods;
	 Hashtbl.add tbl "Normal"   "Class";
	 Hashtbl.add tbl "Abnormal" "Class";
	 tbl
      end
    }
		   
let arffFile llvals =
  let nbFields = FieldNames.number () 
  in {
    header=makeArffHeader "HTTPRequests";
    data=
      Values
	(List.map
	   (fun lvals ->
	    let inst = Array.make nbFields (String "")
	    in begin List.iter (fun (fld,v) -> inst.(FieldNames.index fld) <- v) lvals; inst end)
	   llvals)
  }

let category = ref "Abnormal"		   
let splitData cls n file = Arff.splitData n (fun inst -> inst.(FieldNames.classField)=Enumerated cls) file 
  
