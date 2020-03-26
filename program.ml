type pvar = string
type funName = string

type exp = NULL | Var of string
(* type safeComm = Assign PVar Exp | Malloc PVar *)
(* type unSafeComm = DerefAssign PVar Exp | Modify Exp Exp | Free Exp *)
type bExp = Eeq of exp * exp | Ene of exp * exp
type comm = Assign of pvar * exp | Malloc of pvar
          | Derefer of pvar * exp | Modify of exp * exp | Free of exp
          | Call of funName * exp list | If of bExp * prog * prog
 and prog = comm list

(* string_of_exp : exp -> string *)
let string_of_exp = function
  | NULL -> "NULL"
  | Var x -> x
(* string_of_bExp : bExp -> string *)
let string_of_bExp = function
  | Eeq (e1, e2) -> string_of_exp e1 ^ " == " ^ string_of_exp e2
  | Ene (e1, e2) -> string_of_exp e1 ^ " != " ^ string_of_exp e2
(* string_of_comm : comm -> string *)
let rec string_of_comm_rec n = let ind n = String.make (n * 2) ' ' in function
  | Assign (x, e) -> ind n ^ x ^ " = " ^ string_of_exp e ^ ";\n"
  | Malloc x -> ind n ^ x ^ " = malloc();\n"
  | Derefer (x, e) -> ind n ^ x ^ " = *" ^ string_of_exp e ^ ";\n"
  | Modify (e1, e2) -> ind n ^ "*" ^ string_of_exp e1 ^ " = " ^ string_of_exp e2 ^ ";\n"
  | Free x -> ind n ^ "free(" ^ string_of_exp x ^ ");\n"
  | Call (f, es) -> ind n ^ f ^ "(" ^ String.concat ", " (List.map string_of_exp es) ^ ");\n"
  | If (b, p1, p2) -> ind n ^ "if(" ^ string_of_bExp b ^ ") {\n"
                      ^ string_of_prog_rec (n + 1) p1 ^ "}\n"
                      ^ ind n ^ "else {\n"
                      ^ string_of_prog_rec (n + 1) p2 ^ "}\n"
and string_of_prog_rec n p = String.concat "" (List.map (string_of_comm_rec n) p)
let string_of_comm = string_of_comm_rec 0
let string_of_prog = string_of_prog_rec 0

type fundef = Fun of funName * pvar list * prog
