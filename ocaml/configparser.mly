%token <string> QSTRING
%token FIELD
%token FIELDS
%token EQUALS
%token HEADER
%token DEFAULTMETHOD
%token NGRAMLENGTH
%token MINIMUMNGRAMEAN
%token MINIMUMURLMEAN
%token MINIMUMDISTANCE
%token VERBOSE
%token IDONLY
%token INVERSION
%token END      				  
%token TOKENIZATION
%token NGRAM
%token SUBFIELDS
%token METHOD
%token GLOBALCOUNT
%token FREQUENCYINFIELD
%token SPANISH
%token CATALONIAN
%token ENGLISH
%token DELIMITER
%token RANK
%token MAHALANOBIS
%token NUMBERINFIELD
%token EXCLUDE
%token PRIOR
%token <float> REAL
%token <bool> BOOL
%token <int> NUMBER
%token <string> FIELDNAME
%token EOF
%type <string*string> subfields
%start <Config.t> file

%{

%}

%%
  
file: HEADER hd=header FIELDS l=list(fieldConfig) EOF
  {begin 
    let conf = Config.create ()  
    in  List.iter (fun (fld,fldc) -> Config.add conf fld fldc) l;
	{Config.header=hd;Config.fields=conf}
    end}

header:
DEFAULTMETHOD EQUALS m=tkMethod
NGRAMLENGTH EQUALS n=NUMBER
ar=minngrammean
ad=mindist
VERBOSE EQUALS b1=BOOL
IDONLY  EQUALS b2=BOOL
INVERSION EQUALS b3=BOOL
list(prior)					      
{ Config.defaultMethod:=m;
  Config.length := n;
  Config.minimumNgramMean:=ar;
  Config.minimumDistance:=ad;
 {Config.ngramlength=n;
  Config.verbose=b1;
  Config.idOnly=b2;
  Config.inversion=b3}}

mindist: MINIMUMDISTANCE EQUALS  d=list(REAL) {Array.of_list d}

minngrammean: MINIMUMNGRAMEAN EQUALS lr=list(REAL) {Array.of_list lr}

prior:
PRIOR EQUALS file=QSTRING
{LoadPrior.parser file}
  
fieldConfig:
FIELD
fld=FIELDNAME
subfld=option(FIELDNAME)
tkn=option(tokenInfo)
mthd=option(tokenizationMethod) sbf=option(subfields) minDis=option(mindist) minMean=option(minngrammean)
END
 {{Http.position=Http.FieldNames.index fld;Http.subfield=subfld},
  {Config.Field.tokenization=(match tkn  with Some tk -> tk | None -> (Config.Field.default()).Config.Field.tokenization);
   Config.Field.countmthd   =(match mthd with Some m -> m   | None -> (Config.Field.default()).Config.Field.countmthd);
   Config.Field.subfields=sbf;
   Config.Field.minimumDistance = (match minDis  with Some r -> r | None -> (Config.Field.default()).Config.Field.minimumDistance);
   Config.Field.minimumMean     = (match minMean with Some r -> r | None -> (Config.Field.default()).Config.Field.minimumMean)
   }}

tokenInfo:
 TOKENIZATION tk=tokenizationType  
 {tk}
		   
tokenizationType: 
  NGRAM n=NUMBER {Config.Ngram n}
| DELIMITER str=QSTRING n=NUMBER {Config.Delimiter(str,n)}
| EXCLUDE   {Config.Exclude}			      

tokenizationMethod:
METHOD m=tkMethod	    
 {m}
  
tkMethod:
  RANK           prior=option(lang) {Config.Rank prior}
| MAHALANOBIS    prior=option(lang) {Config.Mahalanobis prior}
| NUMBERINFIELD                     {Config.NumberInField}
| FREQUENCYINFIELD prior=option(lang) {Config.FrequencyInField prior}

lang:
  SPANISH {Config.Spanish}
| CATALONIAN {Config.Catalonian}
| ENGLISH    {Config.English}
			    

subfields: SUBFIELDS sep=QSTRING def=QSTRING {(sep,def)}


