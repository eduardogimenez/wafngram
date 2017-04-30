open Str

let replaceList l str = 
  List.fold_right (fun (r,ns) s -> Str.global_replace (Str.regexp (Str.quote r)) ns s) l str

let escape str = Str.global_replace (Str.regexp "'") "\\'" str

exception StopIter
let forAllChar p str =
  try
    String.iter (fun c -> if p c then raise StopIter) str;
    true
  with StopIter -> false

let isNumber str =
  forAllChar (fun c -> let n = (Char.code c) in 48>n || n>57) str

let isEqual str =
  let x = String.get str 0 in forAllChar (fun c -> c <> x) str
	     
let escape str = str (* Str.global_replace (Str.regexp "'") "\\'" str*)
let removeAccents str =
  String.map
    (function
	 'á' -> 'a'
    |    'é' -> 'e'
    |    'í' -> 'i'
    |    'ó' -> 'o'
    |    'ú' -> 'u'
    |    'ü' -> 'u'
    |    'è' -> 'e'
    |    'à' -> 'a'
    |     ch  ->  ch
    ) str
   
				    
