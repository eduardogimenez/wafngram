%token EOF
%token <int>    NUMBER
%token <string> NGRAM
%token <Config.language> LANGUAGE
%start <unit> file


%{
  let lang = ref Config.Spanish
%}

%%   
file: dico=header list(entry) EOF {FieldModel.ratio (Prior.getDictionnary !lang)}

header: l=LANGUAGE {lang:=l}

entry: ngrm=NGRAM n=NUMBER {Prior.addNgram (Prior.getDictionnary !lang) ngrm n}
