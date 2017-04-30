%token <string> QSTRING
%token FIELD
%token FIELDS
%token EQUALS
%token HEADER
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
NGRAMLENGTH EQUALS n=NUMBER
MINIMUMNGRAMEAN EQUALS lr=list(REAL)
MINIMUMURLMEAN EQUALS   r=REAL
MINIMUMDISTANCE EQUALS  d=list(REAL)
VERBOSE EQUALS b1=BOOL
IDONLY  EQUALS b2=BOOL
INVERSION EQUALS b3=BOOL
list(prior)					      
{{Config.ngramlength=begin Config.length := n;n end;
  Config.minimumNgramMean=Array.of_list lr;
  Config.minimumUrlMean=r;
  Config.minimumDistance=Array.of_list d;
  Config.verbose=b1;
  Config.idOnly=b2;
  Config.inversion=b3}}

prior:
PRIOR EQUALS file=QSTRING
{LoadPrior.parser file}
  
fieldConfig: FIELD fld=NUMBER subfld=option(FIELDNAME) tkn=option(tokenInfo) mthd=option(tokenizationMethod) sbf=option(subfields) END
 {{Http.position=fld;Http.subfield=subfld},
  {Config.tokenization=(match tkn  with Some tk -> tk | None -> (Config.default()).Config.tokenization);
   Config.countmthd   =(match mthd with Some m -> m   | None -> (Config.default()).Config.countmthd);
   Config.subfields=sbf}}

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
  RANK   {Config.Rank}
| GLOBALCOUNT      {Config.GlobalCount}
| NUMBERINFIELD  {Config.NumberInField}
| FREQUENCYINFIELD prior=option(lang) {Config.FrequencyInField prior}

lang:
  SPANISH {Config.Spanish}
| CATALONIAN {Config.Catalonian}
| ENGLISH    {Config.English}
			    

subfields: SUBFIELDS sep=QSTRING def=QSTRING {(sep,def)}


