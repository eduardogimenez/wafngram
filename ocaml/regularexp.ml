open Str

let openseq  = "\\("
let closeseq = "\\)"
let exp s = openseq^s^closeseq
let star s = s^"*"
let plus s = s^"+"
let options l = exp (String.concat "\\|" l)
let seq l = String.concat "" l
let nonemptyseplist x s = seq [s;star (exp (seq [x;s]))]
let seplist x s = star (exp (seq [x;s]))
let opt s = (exp s)^"?"


