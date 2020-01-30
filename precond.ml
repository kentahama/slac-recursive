open Types

(* apply : symbStore -> exp -> term *)
let rec apply s = function
  | NULL -> Nil
  | Var x -> List.assoc x s

(* applyB : symbStore -> bExp -> predicate *)
let applyB s = function
  | Eeq (e1, e2) -> EQ (apply s e1, apply s e2)
  | Ene (e1, e2) -> NE (apply s e1, apply s e2)

(* neg : predicate -> predicate *)
let neg = function
  | EQ (t1, t2) -> NE (t1, t2)
  | NE (t1, t2) -> EQ (t1, t2)

(* mapsto_of : spatFml -> (term * term) list *)
let mapsto_of = List.map (function
                    | Mapsto (t1, t2) -> (t1, t2)
                    | Ls (t1, t2) -> (t1, Hat "end_of_ls")) (* TODO: tilde var *)

(* dom : spatFml -> term list *)
let dom h = List.map fst (mapsto_of h)

(* allocated : pathExp -> spatFml -> term -> bool *)
let allocated d h e = List.mem e (dom h) (* TODO: Use pathexp *)

(* rearrange : pathExp -> spatFml -> term -> term *)
let rearrange d h e = List.assoc e (mapsto_of h)

(* gensym : unit -> string *)
let counter = ref 0
let gensym () =
  counter := !counter + 1;
  "a" ^ string_of_int !counter

(* expand_ls : pathExp -> spatFml -> term -> spatFml *)
let expand_ls d h e = h         (* TODO: impl this *)

(* modify_heap : spatFml -> term -> term -> spatFml*)
let modify_heap h e e' = List.map (function
                             | Mapsto (e1, e2) when e1 = e -> Mapsto (e1, e')
                             | Mapsto (e1, e2) -> Mapsto (e1, e2)
                             | Ls (e1, e2) -> Ls (e1, e2)) h

let string_of_result sdhf_list =
  String.concat "\n" (List.map (fun (s, d, h, f) ->
                          "{" ^ string_of_pureFml (s, d) ^ ", "
                          ^ string_of_spatFml h ^ ", "
                          ^ string_of_spatFml f ^ "}") sdhf_list)

let debug_sdhf sdhf = print_endline (string_of_result sdhf)

let add_footprint (s,d,h,f) e xbar =
  (s, d, Mapsto (e, xbar) :: h, Mapsto (e, xbar) :: f)

(* pre : comm -> symbStore * pathExp * spatFml * spatFml -> (symbStore * pathExp * spatFml * spatFml) list *)
(* preProg : prog -> SH -> SH list *)
let rec pre c (s, d, h, f) = match c with
  | Assign (x, e) ->
     let e' = apply s e in [((x, e') :: s, d, h, f)]
  | Malloc x ->
     let ahat = Hat (gensym ()) and
         ahat' = Hat (gensym ()) in
     [(x, ahat)::s, NE (ahat, Nil) :: d, Mapsto (ahat, ahat') :: h, f]
  | Derefer (x, e) when allocated d h (apply s e) ->
     let e' = apply s e in
     let e'' = rearrange d h e' in
     let h' = expand_ls d h e' in
     [(x, e'')::s, d, h', f]
  | Derefer (x, e) ->
     let es = apply s e in
     let xbar = Bar (gensym ()) in
     let (s, d, h, f) = add_footprint (s, d, h, f) es xbar in
     [(x, xbar)::s, List.map (fun e -> NE (es, e)) (dom h) @ d, h, f]
  | Modify (e, e') when allocated d h (apply s e) ->
     let es = apply s e and
         e's = apply s e' in
     let h' = modify_heap h es e's in
     [s, d, h', f]
  | Modify (e, e') ->
     let es = apply s e and
         e's = apply s e' in
     let e'' = rearrange d h es in
     let xbar = Bar (gensym ()) in
     [add_footprint (s, d, h, f) es xbar]
  | Free e when allocated d h (apply s e) -> failwith "free"
  | Free e -> failwith "free"
  | Call (f, es) -> failwith "call"
  | If (b, p1, p2) -> let b' = applyB s b in
                      preProg p1 (s, b' :: d, h, f) @ preProg p2 (s, (neg b') :: d, h , f)
and preProg p (s, d, h, f) = match p with
  | [] -> [(s, d, h, f)]
  | c :: p -> let rs = pre c (s, d, h, f) in
              print_string (string_of_comm c); debug_sdhf rs;
              List.concat (List.map (fun sdhf -> preProg p sdhf) rs)

(* disp-list (x) { if x == NULL then return else y = *x; disp_list(y); free(x) *)
let dispose_list_body = [
    Malloc "x";
    If (Eeq ((Var "x"), NULL),
      [],
      [Derefer ("y", Var "x"); Call ("disp_list", [Var "y"]); Free (Var "x")])
  ]
(* swap(x, y) = { local t; t = *x; s = *y; *x = s; *y = t; } *)
let swap_body = [
    Derefer ("t", Var "x");
    Derefer ("s", Var "y");
    Modify (Var "x", Var "s");
    Modify (Var "y", Var "t")
  ]

let initSymbStore = ["x", Bar "x"; "y", Bar "y"; "t", Hat "t"; "s", Hat "s"]

let main () =
  print_string (string_of_prog swap_body);
  print_endline "-----";
  debug_sdhf [initSymbStore, [], [], []];
  let res = preProg swap_body (initSymbStore, [], [], []) in
  res
let _ = main ()
