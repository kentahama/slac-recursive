val apply :
  (string * SymbolicHeap.term) list -> Program.exp -> SymbolicHeap.term
val applyB :
  (string * SymbolicHeap.term) list -> Program.bExp -> SymbolicHeap.predicate
val neg : SymbolicHeap.predicate -> SymbolicHeap.predicate
val mapsto_of :
  SymbolicHeap.spatPred list -> (SymbolicHeap.term * SymbolicHeap.term) list
val dom : SymbolicHeap.spatPred list -> SymbolicHeap.term list
val allocated : 'a -> SymbolicHeap.spatPred list -> SymbolicHeap.term -> bool
val rearrange :
  'a -> SymbolicHeap.spatPred list -> SymbolicHeap.term -> SymbolicHeap.term
val counter : int ref
val gensym : unit -> string
val expand_ls : 'a -> 'b -> 'c -> 'b
val modify_heap :
  SymbolicHeap.spatPred list ->
  SymbolicHeap.term -> SymbolicHeap.term -> SymbolicHeap.spatPred list
val string_of_result :
  ((string * SymbolicHeap.term) list * SymbolicHeap.predicate list *
   SymbolicHeap.spatPred list * SymbolicHeap.spatPred list)
  list -> string
val debug_sdhf :
  ((string * SymbolicHeap.term) list * SymbolicHeap.predicate list *
   SymbolicHeap.spatPred list * SymbolicHeap.spatPred list)
  list -> unit
val add_footprint :
  'a * 'b * SymbolicHeap.spatPred list * SymbolicHeap.spatPred list ->
  SymbolicHeap.term ->
  SymbolicHeap.term ->
  'a * 'b * SymbolicHeap.spatPred list * SymbolicHeap.spatPred list
val pre :
  Program.comm ->
  (Program.pvar * SymbolicHeap.term) list * SymbolicHeap.predicate list *
  SymbolicHeap.spatPred list * SymbolicHeap.spatPred list ->
  ((Program.pvar * SymbolicHeap.term) list * SymbolicHeap.predicate list *
   SymbolicHeap.spatPred list * SymbolicHeap.spatPred list)
  list
val preProg :
  Program.prog ->
  (Program.pvar * SymbolicHeap.term) list * SymbolicHeap.predicate list *
  SymbolicHeap.spatPred list * SymbolicHeap.spatPred list ->
  ((Program.pvar * SymbolicHeap.term) list * SymbolicHeap.predicate list *
   SymbolicHeap.spatPred list * SymbolicHeap.spatPred list)
  list
