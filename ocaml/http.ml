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
let printField out fld =
  Printf.fprintf out "%d-%s" fld.position (getSubfield fld.subfield)

let fieldName fld =
  let stringpos fld = (if 0 <= fld.position && fld.position < 10 then "0" else "")^(string_of_int fld.position)
  in  String.concat "-" [stringpos fld;FieldNames.name fld.position;getSubfield fld.subfield]

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


let splitData cls n file = Arff.splitData n (fun inst -> inst.(FieldNames.classField)=Enumerated cls) file 
  
       (*
let encodingPairs = [
      ""   , "%00";
      "\t" , "%09";
      "\n" , "%0A";
      "\r" , "%0D";
      " " ,  "%20";
      "!"  , "%21";
      "\"" , "%22";
      "#" , "%23";
      "$" , "%24";
      "%" , "%25";
      "&" , "%26";
      "\'" , "%27";
      "(" , "%28";
      ")" , "%29";
      "*" , "%2A";
      "+" , "%2B";
      "," , "%2C";
      "-" , "%2D";
      "." , "%2E";
      "/" , "%2F";
      "0" , "%30";
      "1" , "%31";
      "2" , "%32";
      "3" , "%33";
      "4" , "%34";
      "5" , "%35";
      "6" , "%36";
      "7" , "%37";
      "8" , "%38";
      "9" , "%39";
      ":" , "%3A";
      ";" , "%3B";
      "<" , "%3C";
      "=" , "%3D";
      ">" , "%3E";
      "?" , "%3F";
      "@" , "%40";
      "A" , "%41";
      "B" , "%42";
      "C" , "%43";
      "D" , "%44";
      "E" , "%45";
      "F" , "%46";
      "G" , "%47";
      "H" , "%48";
      "I" , "%49";
      "J" , "%4A";
      "K" , "%4B";
      "L" , "%4C";
      "M" , "%4D";
      "N" , "%4E";
      "O" , "%4F";
      "P" , "%50";
      "Q" , "%51";
      "R" , "%52";
      "S" , "%53";
      "T" , "%54";
      "U" , "%55";
      "V" , "%56";
      "W" , "%57";
      "X" , "%58";
      "Y" , "%59";
      "Z" , "%5A";
      "[" , "%5B";
      "\\", "%5C";
      "]" , "%5D";
      "^" , "%5E";
      "_" , "%5F";
      "`" , "%60";
      "a" , "%61";
      "b" , "%62";
      "c" , "%63";
      "d" , "%64";
      "e" , "%65";
      "f" , "%66";
      "g" , "%67";
      "h" , "%68";
      "i" , "%69";
      "j" , "%6A";
      "k" , "%6B";
      "l" , "%6C";
      "m" , "%6D";
      "n" , "%6E";
      "o" , "%6F";
      "p" , "%70";
      "q" , "%71";
      "r" , "%72";
      "s" , "%73";
      "t" , "%74";
      "u" , "%75";
      "v" , "%76";
      "w" , "%77";
      "x" , "%78";
      "y" , "%79";
      "z" , "%7A";
      "{" , "%7B";
      "|" , "%7C";
      "}" , "%7D";
      "~" , "%7E";
      "�" , "%A2";
      "�" , "%A3";
      "�" , "%A5";
      "|" , "%A6";
      "�" , "%A7";
      "�" , "%AB";
      "�" , "%AC";
      "�" , "%AD";
      "�" , "%B0";
      "�" , "%B1";
      "�" , "%B2";
      " , " , "%B4";
      "�" , "%B5";
      "�" , "%BB";
      "�" , "%BC";
      "�" , "%BD";
      "�" , "%BF";
      "�" , "%C0";
      "�" , "%C1";
      "�" , "%C2";
      "�" , "%C3";
      "�" , "%C4";
      "�" , "%C5";
      "�" , "%C6";
      "�" , "%C7";
      "�" , "%C8";
      "�" , "%C9";
      "�" , "%CA";
      "�" , "%CB";
      "�" , "%CC";
      "�" , "%CD";
      "�" , "%CE";
      "�" , "%CF";
      "�" , "%D0";
      "�" , "%D1";
      "�" , "%D2";
      "�" , "%D3";
      "�" , "%D4";
      "�" , "%D5";
      "�" , "%D6";
      "�" , "%D8";
      "�" , "%D9";
      "�" , "%DA";
      "�" , "%DB";
      "�" , "%DC";
      "�" , "%DD";
      "�" , "%DE";
      "�" , "%DF";
      "�" , "%E0";
      "�" , "%E1";
      "�" , "%E2";
      "�" , "%E3";
      "�" , "%E4";
      "�" , "%E5";
      "�" , "%E6";
      "�" , "%E7";
      "�" , "%E8";
      "�" , "%E9";
      "�" , "%EA";
      "�" , "%EB";
      "�" , "%EC";
      "�" , "%ED";
      "�" , "%EE";
      "�" , "%EF";
      "�" , "%F0";
      "�" , "%F1";
      "�" , "%F2";
      "�" , "%F3";
      "�" , "%F4";
      "�" , "%F5";
      "�" , "%F6";
      "�" , "%F7";
      "�" , "%F8";
      "�" , "%F9";
      "�" , "%FA";
      "�" , "%FB";
      "�" , "%FC";
      "�" , "%FD";
      "�" , "%FE";
      "�" , "%FF"]

let uudecodeString str = replaceList encodingPairs str
let uudecodeValue  v   =
  match v with
  | Arff.String str -> Arff.String (uudecodeString str)
  | x               -> x
				   
  
let uudecodeFile file = {
 header = file.header;
 data =
   match file.data with
   | Arff.Values l     -> Arff.Values (List.map (Array.map uudecodeValue) l)
   | Arff.Ocurrences l -> Arff.Ocurrences (List.map (List.map  (fun (fld,v) -> (fld,uudecodeValue v))) l)
}
			*)		      
