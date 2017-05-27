%token EOF
%token <int>    NUMBER
%token <string> NGRAM
%token <Config.language> LANGUAGE
%start <unit> file


%{
  let lang = ref Config.Spanish
%}

%%   
file: dico=header list(entry) EOF
{FieldModel.ratio (Prior.getDictionnary !lang);
 FieldModel.buildFieldModel (Rank (Some !lang)) (Prior.getRank !lang)}

header: l=LANGUAGE {lang:=l}

entry: ngrm=NGRAM n=NUMBER
{Prior.addNgram (Prior.getDictionnary !lang) ngrm n;
 Prior.addNgram (Prior.getRank        !lang) ngrm n}
