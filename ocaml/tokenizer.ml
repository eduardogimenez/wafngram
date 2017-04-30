(********************************************************************************************)
(* Spots the relevant tokens and replace them by identifyable strings                       *)
(********************************************************************************************)
open Str
open Arff
open Regularexp

(********************************************************************************************)
(* Preliminary regular expressions, usefull for several patterns.                           *)
(********************************************************************************************)

(* Lexical class names, indexed by the field number. *)
let lexicalClass str i = "¡¿"^str^"-"^(string_of_int i)^"¡"

(* Blanks *)
let blanks    = star "[' ']"
(* Blanks, including HTTP blank usual encoding. *)
let anyblanks = star "[' '+]"
let atleastoneblank = plus "[' '+]"

(* An identifier *)
let ident    = "[-_a-zA-Z0-9]+"
let rident   = Str.regexp ident
let sident   = lexicalClass "IDENT" 0

(* An IP address *)
let ip = "[0-9][0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?"
let rip = Str.regexp ip
let sip i = lexicalClass "IPADDRESS" i

		
exception BadHTTPEncoding;;
let httpOfStr c =
  if 'A' <= c && c <= 'Z'
  then string_of_int (((Char.code c)-65)+41)
  else if '0' <= c && c <= '9' then string_of_int (((Char.code c)-48)+30)
  else raise BadHTTPEncoding;;
		
(* All the possible ways of encoding the charactar s1, which is encoded in HTTP protocol as s2. 
   The string s2 shall be in uppercase. *)
let encode s1 s2 =
  let lc  = String.lowercase s2 in
  let c2   = String.get s2 0 in
  let c3   = String.get s2 1 in
  let cc11 = "%" in
  let cc12 = "%25" in
  let cc21 = String.make 1 c2   in
  let cc22 = "%"^(httpOfStr c2) in
  let cc31 = String.make 1 c3   in
  let cc32 = "%"^(httpOfStr c3)  
  in options [s1;cc11^s2;cc11^lc;cc12^cc21^cc31;cc12^cc21^cc32;cc12^cc22^cc31;cc12^cc22^cc32]
		
let lpar   = encode "(" "28"
let rpar   = encode ")" "29"
let lsbrk  = encode "]" "5B"
let rsbrk  = encode "[" "5C"
let lpic   = encode "<" "3C"
let rpic   = encode ">" "3E"
let semicolon = encode ";" "3B"
let bar       = encode "|" "7C"
let slash     = encode "/" "2F"
let backslash = encode "\\\\" "5C"
let dot       = encode "\\."  "2E"
let return = options ["\\r";encode "\\n" "0A"]
let dash   = encode "#" "23"
let emark  = encode "!" "21"
let minus  = encode "-" "2D"

(* Escape characters which are frequently used to open a sentence in another language 
   in injection attacks. *)
let escape  = options [backslash;"'"] (* "\\(\\\"\\|\\\'\\)" *)
let rescape   = Str.regexp escape
let sescape i = lexicalClass "ESC" i

		    
(* Unused. 

(* An URL *)
let rurl = Str.regexp "\\(http://\\|http://www\\|www\\)\\(\\.[-_a-zA-Z0-9]+\\)+\\(/[-_a-zA-Z0-9]*\\)*\\(:[0-9]+\\)?"
let surl = "¡¿URL¡"


(* An URL with hardcoded IP *)
let rurlhc = Str.regexp ("http://"^ip^"\\(/[-_a-zA-Z0-9/]\\)*\\(:[0-9]+\\)?")
let surlhc = "¡¿URL-HARDCODED-IP¡"

(* Resetting cookies *)
let rsetcookie = Str.regexp "Set-cookie"
let ssetcookie = "¡¿SETCOOKIE¡"

(* An e-mail address *)
let remail    = Str.regexp "[-_\\.a-zA-ZÀ-ÿ0-9]+@[-_\\.a-zA-ZÀ-ÿ0-9]+"
let semail    = "¡¿EMAIL¡"

(* Encoded characters, such as %3E, etc. *)
let rencodechar  = Str.regexp "%[0-9A-F][0-9A-F]"
let sencodechar = "¡¿ENCODEDCHAR¡"

(* The left hand side of a definition. *)
let rdefinition   = Str.regexp "[a-zA-Z0-9]+="

(* Text *)
let rtext = Str.regexp "^[a-zA-ZÀ-ÿ\\+]+$\\|[a-zA-ZÀ-ÿ\\+]+\\+\\|\\+[a-zA-ZÀ-ÿ\\+]+"
let stext = "\\1¡¿TEXT¡"

(* Numbers *)
let rnumber = Str.regexp "[0-9]+"
let snumber = "¡¿NUMBER¡"

(* 
 * Alphanumeric strings, separated by commas or "+", at the beggining or end of the string, or in between separation "+".
 *  We need this complicated rule to avoid replacing the strings standing for classes. 
 *)
let ralphanum = Str.regexp "^[a-zA-ZÀ-ÿ0-9,\\+]+\\|[, :&'\n'\\+][a-zA-ZÀ-ÿ0-9,\\+]+"
let salphanum = "¡¿ALPHANUM¡"

(* E-Tags bien formados, para disminuir la cantidad de variables. *)
let retag = Str.regexp "\\(W/\\)?\"[-_a-zA-Z0-9.]*\""
let setag = "¡¿ETAG¡"

(* The value preceeding the attack. *)
let rthevalue = Str.regexp "^[^ ' '¡$(<'\\\''\'''\"']+"
let sthevalue = "¡¿VALUE¡"
		  

 *)
       
(********************************************************************************************)
(* SQL Injection                                                                            *)
(********************************************************************************************)

(* Null comments shall be skipped *)
let rnullcomment = Str.regexp "/\\*\\*/"
let snullcomment = ""

let sqlprefix   = options [lpar;rpar;atleastoneblank;slash;"¡";"\'";"\"";semicolon] (* [()' '\\/+¡'\'''\"';] *)
let sqlposfix   = options [atleastoneblank;lpar;slash;backslash;"¡"] (* [(' '\\/+] *)
let sqlupperkeywords =
  options ["ADD";"EXTERNAL";"PROCEDURE";"ALL";"FETCH";"PUBLIC";"ALTER";"FILE";"RAISERROR";"AND";"FILLFACTOR";"READ";"ANY";"FOR";"READTEXT";"AS";"FOREIGN";"RECONFIGURE";"ASC";"FREETEXT";"REFERENCES";"AUTHORIZATION";"FREETEXTTABLE";"REPLICATION";"BACKUP";"FROM";"RESTORE";"BEGIN";"FULL";"RESTRICT";"BETWEEN";"FUNCTION";"RETURN";"BREAK";"GOTO";"REVERT";"BROWSE";"GRANT";"REVOKE";"BULK";"GROUP";"RIGHT";"BY";"HAVING";"ROLLBACK";"CASCADE";"HOLDLOCK";"ROWCOUNT";"CASE";"IDENTITY";"ROWGUIDCOL";"CHECK";"IDENTITY_INSERT";"RULE";"CHECKPOINT";"IDENTITYCOL";"SAVE";"CLOSE";"IF";"SCHEMA";"CLUSTERED";"IN";"SECURITYAUDIT";"COALESCE";"INDEX";"SELECT";"COLLATE";"INNER";"SEMANTICKEYPHRASETABLE";"COLUMN";"INSERT";"SEMANTICSIMILARITYDETAILSTABLE";"COMMIT";"INTERSECT";"SEMANTICSIMILARITYTABLE";"COMPUTE";"INTO";"SESSION_USER";"CONSTRAINT";"IS";"SET";"CONTAINS";"JOIN";"SETUSER";"CONTAINSTABLE";"KEY";"SHUTDOWN";"CONTINUE";"KILL";"SOME";"CONVERT";"LEFT";"STATISTICS";"CREATE";"LIKE";"SYSTEM_USER";"CROSS";"LINENO";"TABLE";"CURRENT";"LOAD";"TABLESAMPLE";"CURRENT_DATE";"MERGE";"TEXTSIZE";"CURRENT_TIME";"NATIONAL";"THEN";"CURRENT_TIMESTAMP";"NOCHECK";"TO";"CURRENT_USER";"NONCLUSTERED";"TOP";"CURSOR";"NOT";"TRAN";"DATABASE";"NULL";"TRANSACTION";"DBCC";"NULLIF";"TRIGGER";"DEALLOCATE";"OF";"TRUNCATE";"DECLARE";"OFF";"TRY_CONVERT";"DEFAULT";"OFFSETS";"TSEQUAL";"DELETE";"ON";"UNION";"DENY";"OPEN";"UNIQUE";"DESC";"OPENDATASOURCE";"UNPIVOT";"DISK";"OPENQUERY";"UPDATE";"DISTINCT";"OPENROWSET";"UPDATETEXT";"DISTRIBUTED";"OPENXML";"USE";"DOUBLE";"OPTION";"USER";"DROP";"OR";"VALUES";"DUMP";"ORDER";"VARYING";"ELSE";"OUTER";"VIEW";"END";"OVER";"WAITFOR";"ERRLVL";"PERCENT";"WHEN";"ESCAPE";"PIVOT";"WHERE";"EXCEPT";"PLAN";"WHILE";"(*EXEC";"*)PRECISION";"WITH";"EXECUTE";"PRIMARY";"WITHIN GROUP";"EXISTS";"PRINT";"WRITETEXT";"EXIT";"PROC"]
let sqllowerkeywords = (String.lowercase sqlupperkeywords)
let sqlkeywords = options [sqlupperkeywords;sqllowerkeywords]
let sqlsequence = seq [sqlprefix;sqlkeywords;sqlposfix]
	      
(* SQL keywords *)     
let rsqlkeyword   = Str.regexp sqlsequence
let ssqlkeyword i = lexicalClass "SQLKEYWORD" i

(********************************************************************************************)
(* Logical OR, used by several languages - interference.                                    *)
(********************************************************************************************)
		    
(* Augmenting logical connective *)
let rorconnective   = Str.regexp "^\\(or\\|OR\\||\\)[' ']\\|[' ']\\(or\\|OR\\||\\)"
let sorconnective i = lexicalClass "OR" i

(* Directives for executing javascript code *)
let rexec      = Str.regexp "javascript:"
let sexec i    = lexicalClass "JAVASCRIPT"

(********************************************************************************************)
(* Path traversal                                                                           *)
(********************************************************************************************)

let filesep   = plus (options [slash;backslash])
let filename = options [ident;dot]
let filepath s = seq [s;nonemptyseplist s filename]
let winfilepath = seq [backslash;nonemptyseplist backslash filename]
let rpath   = Str.regexp (filepath slash)
let spath i = lexicalClass "FILEPATH" i

let disruptdir = options [dot;seq [dot;dot];seq [dot;dot;dot]];;
let disruptivefilepath s = seq [nonemptyseplist s disruptdir;s;filename];;
let rexec      = Str.regexp (disruptivefilepath filesep);;
let sexec i    = lexicalClass "DISRUPTIVEFILEPATH" i;;
let rootpath   = seq ["^";opt dot;plus slash;"$"]
let rrootpath    = Str.regexp rootpath
let srootpath  i = lexicalClass "ROOTPATH" i
let autoexec   = seq [opt "[a-zA-Z]:";"\\\\\\\\";"autoexec\\.bat"]
let rautoexec  = Str.regexp autoexec
let sautoexec  i = lexicalClass "AUTOEXECDOTBAT" i
let winresource   = "[A-Za-z0-9]:"
let winrootpath = seq [(*opt winresource;*)options ["WINNT";"WINDOWS";"winnt";"windows"];filepath filesep]
let rwinrootpath = Str.regexp winrootpath
let swinrootpath i = lexicalClass "WINROOTPATH" i
let absolutfilename = seq ["file:///";winresource;filepath slash]
let rabsfilename = Str.regexp absolutfilename
let sabsfilename i = lexicalClass "WINABSFILENAME" i

(********************************************************************************************)
(* SSI patterns                                                                             *)
(********************************************************************************************)

(* Directives for executing code *)
let ssiopen   = seq [options[lpic;lpar];emark;minus;minus]
let ssiclose  = seq [minus;minus;options[rpic;rpar]]
let ssikywrd  = seq[dash;options ["[Ee][Xx][Ee][Cc]";"[Ii][Nn][Cc][Ll][Uu][Dd]";"[Oo][Dd][Bb][Cc]"]]

let rssiopen    = Str.regexp ssiopen
let sssiopen  i = lexicalClass "SSIOPEN" i
let rssiclose   = Str.regexp ssiclose
let sssiclose i = lexicalClass "SSICLOSE" i
let rssikywrd   = Str.regexp ssikywrd
let sssikywrd i = lexicalClass "SSIKEYWORD" i
		  
(********************************************************************************************)
(* XSS patterns                                                                             *)
(********************************************************************************************)
	       
(* Opening tag for a piece of executable script in XSS attacks *)
let btag s=
  seq [
      options ["%3C";"<";"(";"lt;";"\\[\\\\\\\\xBC\\]"]; (* can't use &lt; because of &, which is splitted. [xBC] doesn't work*)
      blanks;
      s;
      blanks;
      options ["%3E";">";"glt;"]]
(* Ending tag for a piece of executable script in XSS attacks *)
let etag s =
  seq [
      options [lpic;"(";"lt;";"\\[\\\\\\\\xBC\\]"];
      blanks;
      "/";
      s;
      blanks;
      options [rpic;"gt;"]]
let scriptbtag = btag "[Ss][Cc][Rr][Ii][Pp][Tt]"
let scriptetag = etag "[Ss][Cc][Rr][Ii][Pp][Tt]"
      
let rtagbscript    = Str.regexp scriptbtag
let stagbscript i  = lexicalClass "SCRIPT-B" i
let rtagescript    = Str.regexp scriptetag
let stagescript i  = lexicalClass "SCRIPT-E" i

let jsEvents =
  options ["onchange";"onclick";"onmouseover";"onmouseout";"onkeydown";"onload"] (* to be completed *)
let rjsEvents   = Str.regexp jsEvents
let sjsEvents i = lexicalClass "JSEVENT" i
	  
let jsdocumentProperties =
  seq ["document\\.";options ["cookie";"domain"]] (*to be completed *)
let windowMethods    =
 seq [options ["alert";"url";"window.open"];"[' ']*("](* to be completed *) 
let locationMethods =
  seq ["document\\.location\\.";options ["assign";"relod";"replace"];"[' ']*("] (* to be completed *)
let jscode  = options ["javascript";jsdocumentProperties;windowMethods;locationMethods]
let rjscode   = Str.regexp jscode
let sjscode i = lexicalClass "JSCODE" i

let htmltag    = btag (options ["[Dd][Ii][Vv]";"[[Ii][Mm][Gg]"]) (* to be completed *)
let rhtmltag   = Str.regexp htmltag
let shtmltag i = lexicalClass "HTMLTAG" i
		  
(********************************************************************************************)
(* XPath patterns                                                                           *)
(********************************************************************************************)
	       
let nodename = "\\([A-Za-z0-9]+\\|\\*\\)"
let axis =
  seq
    [options
       ["ancestor";"ancestor-of-self";"attribute";"child";"descendant";"descendant-of-self";"following";
	"following-sibling";"namespace";"parent";"preceeding";"preceeding-sibling";"self"]; 
     "::"]
let noargs = seq [lpar;star " ";rpar]
let predicate = "\\[[^]]+\\]"
let funapp = seq [options ["node";"text";"comment"];noargs]
let node = options [nodename;"@"^nodename;"\\.";"\\.\\.";funapp]
let funxpath = seq [opt axis;node;slash;funapp;opt predicate]
let predicatexpath = seq [opt axis;node; predicate]
let axisxpath = seq [axis;node;opt predicate]
let countxpath = "count("
let xpath = options [countxpath;funxpath;predicatexpath;axisxpath;"or ";"| "]
let rxpath   = Str.regexp xpath
let sxpath i = lexicalClass "XPATH" i

(********************************************************************************************)
(* OS Commanding patterns                                                                   *)
(********************************************************************************************)
      
let osescape    = options [semicolon;bar;return;seq [backslash;semicolon];"/c";"=";"^"]
let rosescape   = Str.regexp osescape
let sosescape i = lexicalClass "OSESC" i

let atleastoneblank  = options [plus "[' '+]";"|";";";"%3b";"$"]

let oscommand =
  seq [opt (options ["/usr/bin/";"/bin/";
		     seq[backslash;slash;"usr";backslash;slash;"bin";backslash;slash];
		     seq[backslash;slash;"bin";backslash;slash]]);
       options ["tftp";"echo";"mail";"rm";"cat";"nc";"nftp.exe";"wget";"ps";"uname";"xterm";
		"telnet";"id";"uftp";"perl";"/perl";"dir";"echo";"ls";"copy";"cmd.exe";"shell"]
      ]

(* mail e id con \\bin antes *)

let oskeyword    = seq [osescape;anyblanks;oscommand;atleastoneblank]
let roskeyword   = Str.regexp oskeyword
let soskeyword i = lexicalClass "OSKEYWORD" i

let ostarget    = options ["/etc/passwd";"/var/log/httpd/access[_.]log";"/etc/httpd/httpd.conf";"bash_history"]
let rostarget   = Str.regexp ostarget
let sostarget i = lexicalClass "OSTARGET" i
		   
let osflags    = options ["-aux";"-f";"-rf";"-vvv";"-las"]
let rosflags   = Str.regexp osflags
let sosflags i = lexicalClass "OSFLAGS" i

let exefile    = seq [filepath slash;options ["\\.exe";"\\.pl";"\\.bat"]]
let rexefile   = Str.regexp exefile
let sexefile i = lexicalClass "OSEXEFILE" i

let shellexec    = "shell("
let rshellexec   = Str.regexp shellexec
let sshellexec i = lexicalClass "SHELLEXEC" i

(********************************************************************************************)
(* LDAP Injection patterns                                                                  *)
(********************************************************************************************)
let ldapvalue      = (* "[A-Za-z0-9'* ]+" *) "[^) ]+"
let ldapprefix    = seq [options ["$";"="];opt ldapvalue;blanks;")+"]
let rldapprefix   = Str.regexp ldapprefix
let sldapprefix i = lexicalClass "LDAPPREFIX" i
  
let ldappredicate = seq ["(";blanks;ident;blanks;"=";blanks;ldapvalue;blanks;")"]
let rldapatom   = Str.regexp ldappredicate
let sldapatom i = lexicalClass "LDAPATOM" i

let ldapandpredicate = seq ["[&]";blanks;"(";blanks;ident;blanks;"=";blanks;ldapvalue;")"]
let rldapandatom   = Str.regexp ldapandpredicate
let sldapandatom i = (lexicalClass "LDAPAND" i)^(lexicalClass "LDAPATOM" i)

let ldaporpredicate = seq ["[|]";blanks;"(";blanks;ident;blanks;"=";blanks;ldapvalue;")"]
let rldaporatom   = Str.regexp ldaporpredicate
let sldaporatom i = (lexicalClass "LDAPOR" i)^(lexicalClass "LDAPATOM" i)

(* reorganizar para no forzosamente cortar con "&" *)
		   
(********************************************************************************************)
(* All replacements to be performed.                                                        *)
(********************************************************************************************)
	 
let pathTraversalReplacements i = [
  (rautoexec,sautoexec i);
  (rexec,sexec i);
  (rwinrootpath,swinrootpath i);
  (rabsfilename,sabsfilename i);
  (rrootpath,srootpath i)
]
let ldapInjectionReplacements i = [
    (rldapprefix,sldapprefix i);
    (rldapatom,sldapatom i);
    (rldaporatom,sldaporatom i);
    (rldapandatom,sldapandatom i)
]
let osCommandingReplacements i = [
    (rip,sip i);
    (rexec,sexec i);
    (rexefile,sexefile i);
    (rosflags,sosflags i);
    (rostarget,sostarget i);
    (roskeyword,soskeyword i)
]
let xssReplacements i = [			    
  (rtagbscript,stagbscript i);
  (rtagescript,stagescript i);
  (rjsEvents,sjsEvents i);
  (rjscode,sjscode i);
  (rhtmltag,shtmltag i)
]
let ssiReplacements i = [
  (rssiopen,sssiopen i);
  (rssiclose,sssiclose i);
  (rssikywrd,sssikywrd i)
]
let xpathReplacements i = [
   (rxpath, sxpath i);
   (rescape,sescape i)
]
let sqlInjectionReplacements i = [
   (rsqlkeyword, ssqlkeyword i);
   (rnullcomment,snullcomment);
   (rsqlkeyword, ssqlkeyword i);
   (rescape,sescape i)
]
let replacements i =
  List.concat [
      pathTraversalReplacements i;
      ldapInjectionReplacements i;
      osCommandingReplacements  i;
      xssReplacements i;
      ssiReplacements i;
      xpathReplacements i;
      sqlInjectionReplacements i
    ]

(********************************************************************************************)
(* Replacement algorithm.                                                                   *)
(********************************************************************************************)
	      
(********************************************************************************************)
(* Side effects.                                                                                 *)
(* An array specifying for each index attribute whether it is a monolitic field 
   or whether it is made of subfields separated by defSep string and asigned by 
   defSym string. *)
let subfields : ((string*string) option) array = Array.make 100 None
(* Abstracts away subfield identifiers when set. *)
let identTransform = ref true
(* Keeps the subfield identifiers when set *)
let identKeep = ref true
(********************************************************************************************)
		    
(* Sequentially applies all the defined replacements on a given string *)
let abstractOneField i = 
  List.fold_right
    (fun (r,ns) s -> global_replace r ns s)
    (replacements i)

(* Takes a string which is an id=value subfield and perform the replacements on value.
   If identKeep is set, keeps the subfield identifier id.
   If identTransform is set, the subfield identifier is also abstracted away. *)
let replaceString i defsym s =
  match Str.split (Str.regexp defsym) s with
  | [def;den] ->
     if not !identKeep
     then abstractOneField i den 
     else String.concat defsym [if !identTransform then sident else def;abstractOneField i den]
  | _         ->  abstractOneField i s

(* Splits a string into a list of strings delimited by defsep and performs replacements 
   on subfields asigned with symbol defsym. The resulting subfields are separated by "¡". *)
let splitAndReplace i (defsep,defsym) s =
  let strl = Str.split (Str.regexp defsep) s in
  let absl = List.map (replaceString i defsym) strl
  in  String.concat " ¡ " absl
     
(* Performs replacements on the fields in the rank [n,m] of instance inst.
   If the field has subfields, replacements are performed only in the values.
   Raises BoundsDoesNotFormARank if n => m. *)
exception BoundsDoesNotFormARank of int*int
let abstractStringFields n m inst =
  begin
    if n <= m then
      for i = n to m do
	match inst.(i) with
	| String str ->
	   (match subfields.(i) with
	    | None      -> inst.(i) <- String (abstractOneField 0 str)
	    | Some defs -> inst.(i) <- String (splitAndReplace  0 defs str))
	| _ -> ()
      done
    else raise (BoundsDoesNotFormARank (n,m));
    inst
  end

(* Performs replacements in the fileds inside the rank [n,m] of all the ARFF file instances. *)
let process file n m =
  {header = file.header;
   data   =
     match file.data with
     | Values l -> Values (List.map (abstractStringFields n m) l)
     | x -> x
  }

(********************************************************************************************)
(********************************************************************************************)
 
