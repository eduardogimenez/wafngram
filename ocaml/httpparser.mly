%token EOF
%token <string> METHOD 
%token <string> URI
%token <string> PROTOCOL
%token <string> FIELDNAME
%token <string> FIELDCONTENTS
%token <string> BODY
%start <Arff.t> data

%{
let id = ref (-1)
%}

%%
request: h=header flds=fields bd=body
{id:=!id+1;
 List.concat [[("Identifier",Arff.Number !id);("Class",Arff.Enumerated !Http.category)];h;flds;[bd]]}
				   
header: mthd=METHOD uri=URI protocol=PROTOCOL
{ ("Method",  Arff.Enumerated mthd)::
  ("Protocol",Arff.Enumerated protocol)::
    match Str.bounded_split (Str.regexp "?") uri 2 with
    | [uri;query] ->
       [("Uri",   Arff.String (Http.decode uri));
        ("Query", Arff.String (Http.decode query))]
    | [uri] ->
       [("Uri",   Arff.String (Http.decode uri));
        ("Query", Arff.String "")]
    | _ -> raise (Invalid_argument "bounded_split")}			  

field: field=FIELDNAME contents=FIELDCONTENTS {(field,Arff.String (Netencoding.Url.decode contents))}

fields: l=nonempty_list(field) {l}	   

body: contents=BODY {("Body",Arff.String (Http.decode contents))}
			   
data: l=list(request) EOF {Printf.fprintf stdout "%d instances were parsed\n\n" (List.length l);Http.arffFile l}
