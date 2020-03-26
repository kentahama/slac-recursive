type term = Nil | Bar of string | Hat of string
type predicate = EQ of term * term | NE of term * term
type pathExp = predicate list
type symbStore = (Program.pvar * term) list
type pureFml = symbStore * pathExp
type spatPred = Mapsto of term * term | Ls of term * term
type spatFml = spatPred list
type symbHeap = pureFml * spatFml

val string_of_term : term -> string
val string_of_predicate : predicate -> string
val string_of_pathExp : predicate list -> string
val string_of_symbStore : (string * term) list -> string
val string_of_pureFml : (string * term) list * predicate list -> string
val string_of_spatPred : spatPred -> string
val string_of_spatFml : spatPred list -> string
val string_of_symbHeap :
  ((string * term) list * predicate list) * spatPred list -> string
