type term = Nil | Bar of string | Hat of string
type predicate = EQ of term * term | NE of term * term
type pathExp = predicate list
type symbStore = (Program.pvar * term) list
type pureFml = symbStore * pathExp
type spatPred = Mapsto of term * term | Ls of term * term
type spatFml = spatPred list
type symbHeap = pureFml * spatFml

(* string_of_term : term -> string *)
let string_of_term = function
  | Nil -> "nil"
  | Bar x -> "_" ^ x
  | Hat a -> "^" ^ a
(* string_of_predicate : predicate -> string *)
let string_of_predicate = function
  | EQ (t1, t2) -> string_of_term t1 ^ " = " ^ string_of_term t2
  | NE (t1, t2) -> string_of_term t1 ^ " != " ^ string_of_term t2
(* string_of_pathExp : pathExp -> string *)
let string_of_pathExp p =
  String.concat " & " (List.map string_of_predicate p)
(* string_of_symbStore : symbStore -> string *)
let string_of_symbStore s =
  String.concat "; " (List.map (fun (x, e) -> x ^ ":=" ^ string_of_term e) s)
(* string_of_pureFml : pureFml -> string *)
let string_of_pureFml (s, d) = string_of_symbStore s ^ " & " ^ string_of_pathExp d
(* string_of_spatPred : spatPred -> string *)
let string_of_spatPred = function
  | Mapsto (e1, e2) -> string_of_term e1 ^ " |-> " ^ string_of_term e2
  | Ls (e1, e2) -> "ls(" ^ string_of_term e1 ^ ", " ^ string_of_term e2 ^ ")"
(* string_of_spatFml : spatFml -> string *)
let string_of_spatFml h = String.concat " * " (List.map string_of_spatPred h)
(* string_of_symbHeap : symbHeap -> string *)
let string_of_symbHeap (p, s) = string_of_pureFml p ^ " | " ^ string_of_spatFml s
