%token EOF
%token <string> METHOD 
%token <string> URI
%token <string> PROTOCOL
%token <string> FIELDNAME
%token <string> FIELDCONTENTS
%token <string> BODY
%token <int> REQIDENT
%token BEGINREQ
%token ENDREQ
%token CLASSTOKEN
%token <string> CLASS
%start <Arff.t> data
%%

request: BEGINREQ n=FIELDCONTENTS CLASSTOKEN cl=CLASS h=header flds=fields bd=body ENDREQ FIELDCONTENTS
  {List.concat [h;flds;[bd];[("Identifier", Arff.Number (int_of_string n));("Class",Arff.Enumerated (Http.FieldNames.mapClassValue cl))] ]}
				   
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

field: field=FIELDNAME opt=option(FIELDCONTENTS)
{begin
    Http.FieldNames.add field;
    let contents = match opt with None -> "" | Some str -> str in (field, Arff.String (Http.decode contents))
 end}

fields: l=nonempty_list(field) {l}	   

body: contents=BODY {("Body",Arff.String (Http.decode contents))}
			   
data: l=list(request) EOF {Printf.fprintf stdout "%d instances were parsed\n\n" (List.length l);Http.arffFile l}
