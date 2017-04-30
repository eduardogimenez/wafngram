%token EOF
%token <string> METHOD 
%token <string> URI
%token <string> PROTOCOL
%token <string> FIELDNAME
%token <string> FIELDCONTENTS
%token <string> BODY
%start <Arff.t> data
%%

request: h=header flds=fields bd=body {List.concat [h;flds;[bd];[("Class",Arff.Enumerated "Valid")]]}
				   
header: mthd=METHOD uri=URI protocol=PROTOCOL
{ ("Method",  Arff.Enumerated mthd)::
  ("Protocol",Arff.Enumerated protocol)::
    match Str.bounded_split (Str.regexp "?") uri 2 with
    | [uri;query] ->
       [("Uri",   Arff.String uri);
        ("Query", Arff.String query)]
    | [uri] ->
       [("Uri",   Arff.String uri);
        ("Query", Arff.String "")]
    | _ -> raise (Invalid_argument "bounded_split")}			  

field: field=FIELDNAME contents=FIELDCONTENTS {(field,Arff.String (Netencoding.Url.decode contents))}

fields: l=nonempty_list(field) {l}	   

body: contents=BODY {("Body",Arff.String contents)}
			   
data: l=list(request) EOF {Printf.fprintf stdout "%d instances were parsed\n\n" (List.length l);Http.arffFile l}
