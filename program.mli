type pvar = string
type funName = string
type exp = NULL | Var of string
type bExp = Eeq of exp * exp | Ene of exp * exp
type comm =
    Assign of pvar * exp
  | Malloc of pvar
  | Derefer of pvar * exp
  | Modify of exp * exp
  | Free of exp
  | Call of funName * exp list
  | If of bExp * prog * prog
and prog = comm list
type fundef = Fun of funName * pvar list * prog

val string_of_exp : exp -> string
val string_of_bExp : bExp -> string
val string_of_comm_rec : int -> comm -> string
val string_of_prog_rec : int -> prog -> string
val string_of_comm : comm -> string
val string_of_prog : prog -> string
