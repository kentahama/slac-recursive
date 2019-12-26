type pvar = string
type term = Nil | Bar of string | Hat of string
type predicate = EQ of term * term | NE of term * term
type pathExp = predicate list
type symbStore = (pvar * term) list
type pureFml = symbStore * pathExp
type spatPred = Mapsto of term * term | Ls of term * term
type spatFml = spatPred list
type symbHeap = pureFml * spatFml

(* string_of_term : term -> string *)
let string_of_term = function
  | Nil -> "nil"
  | Bar x -> "\\bar " ^ x
  | Hat a -> "\\hat " ^ x
(* string_of_predicate : predicate -> string *)
let string_of_predicate = function
  | EQ t1 t2 -> string_of_term t1 ^ "=" ^ string_of_term t2
  | NE t1 t2 -> string_of_term t1 ^ "!=" ^ string_of_term t2
(*  *)
              
type funName = string

type exp = NULL | Var of string
(* type safeComm = Assign PVar Exp | Malloc PVar *)
(* type unSafeComm = DerefAssign PVar Exp | Modify Exp Exp | Free Exp *)
type bExp = Eeq of exp * exp | Ene of exp * exp
type comm = Assign of pvar * exp | Malloc of pvar
          | Derefer of pvar * exp | Modify of exp * exp | Free of exp
          | Call of funName * exp list | If of bExp * prog * prog
 and prog = comm list

type fundef = Fun of funName * pvar list * prog

(* lookup : 'a -> ('a * 'b) list -> 'b *)
let lookup x ps = snd (List.find (fun p -> x = fst p) ps)

(* apply : symbStore -> exp -> term *)
let rec apply s = function
  | NULL -> Nil
  | Var x -> lookup x s

(* applyB : symbStore -> bExp -> predicate *)
let applyB s = function
  | Eeq (e1, e2) -> EQ (apply s e1, apply s e2)
  | Ene (e1, e2) -> NE (apply s e1, apply s e2)

(* neg : predicate -> predicate *)
let neg = function
  | EQ (t1, t2) -> NE (t1, t2)
  | NE (t1, t2) -> EQ (t1, t2)

(* dom : spatFml -> term list *)
let dom h = List.map function
              | Mapsto (t1, t2) -> t1
              | Ls (t1, t2) -> t1

(* allocated : pathExp -> spatFml -> exp -> bool *)
let allocated d h e =
  let es = dom h in
  mem e es

let rearrange d h e =
  let es = ???
  
(* gensym : unit -> string *)
let counter = ref 0
let gensym () =
  counter := !counter + 1;
  "a" ^ string_of_int !counter

(* pre : comm -> symbStore * pathExp * spatFml * spatFml -> (symbStore * pathExp * spatFml * spatFml) list *)
(* preProg : prog -> SH -> SH list *)
let rec pre c (s, d, h, f) = match c with
  | Assign (x, e) -> let e' = apply s e in [((x, e') :: s, d, h, f)]
  | Malloc x -> let ahat = Hat (gensym ()) and
                    ahat' = Hat (gensym ()) in
                [(x, ahat)::s, NE (ahat, Nil) :: d, Mapsto (ahat, ahat') :: h, f]
  | Derefer (x, e) when allocated d h e -> []
  | Derefer (x, e) -> failwith "unalloc derefer"
  | Modify (e, e') -> failwith "modify"
  | Free e -> failwith "free"
  | Call (f, es) -> failwith "call"
  | If (b, p1, p2) -> let b' = applyB s b in
                      preProg p1 (s, b' :: d, h, f) @ preProg p2 (s, (neg b') :: d, h , f)
and preProg p (s, d, h, f) = match p with
  | [] -> [(s, d, h, f)]
  | c :: p -> let rs = pre c (s, d, h, f) in
              List.concat (List.map (fun sdhf -> preProg p sdhf) rs)

(* disp-list (x) { if x == NULL then return else y = *x; disp_list(y); free(x) *)


let dispose_list_body = [
    If (Eeq ((Var "x"), NULL), 
      [],
      [Derefer ("y", Var "x"); Call ("disp_list", [Var "y"]); Free (Var "x")])
  ]    

let initSymbStore = ["x", Bar "x"; "y", Hat "y"]
let main () = preProg dispose_list_body (initSymbStore, [], [], [])
let _ = main ()
