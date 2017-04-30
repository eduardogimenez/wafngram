%token <string> STRING
%token <string> QSTRING
%token RELATION
%token ATTRIBUTE				  
%token DATA
%token LBRACKET
%token RBRACKET
%token NUMERICTYPE
%token INTEGERTYPE
%token REALTYPE
%token STRINGTYPE
%token DATETYPE
%token COMMA
%token QMARK
%token <int> INT
%token EOF
%token <float> FLOAT       
%start <Arff.t> file
%type <string*Arff.attributeType> adecl

%{
  let cnsTable = Hashtbl.create 100

%}

%%
  
file: h=header d=data EOF {{Arff.header=h;Arff.data=d}}

header: RELATION str=string ad=nonempty_list(adecl){
  {Arff.relname=str;
   Arff.attributes=ad;
   Arff.constants=cnsTable}
}

adecl: 
ATTRIBUTE s=string t=arfftype
{begin
    (match t with
     | Arff.EnumeratedType l -> List.iter (fun cns -> Hashtbl.add cnsTable cns s) l
     | n_ -> ());
    (s,t)
end}

arfftype:
 NUMERICTYPE {Arff.NumberType}
|INTEGERTYPE{Arff.IntegerType}
|REALTYPE{Arff.FloatType}
|STRINGTYPE{Arff.StringType}
|DATETYPE {Arff.DateType}
|LBRACKET l=separated_list(COMMA,string) RBRACKET {Arff.EnumeratedType l}

data:
  DATA LBRACKET l=nonempty_list(nonempty_list(pair(INT,value))) RBRACKET {Arff.Ocurrences l}
| DATA l=nonempty_list(instance) {Arff.Values l}   

instance:
  l=separated_nonempty_list(COMMA,value) {Array.of_list l}

value: QMARK {Arff.Missing} | r=FLOAT {Arff.Float r} | i=INT
  {Arff.Integer i} | s=QSTRING {Arff.String s} | s=STRING {if
  Hashtbl.mem cnsTable s then Arff.Enumerated s else Arff.String s}

string:
  s=QSTRING  {s}
| s=STRING   {s}

