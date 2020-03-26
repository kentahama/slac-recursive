module S = SymbolicHeap
open Program

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
let swap2_body = [
    Modify (Var "x", Var "s");
    Modify (Var "y", Var "t");
    Derefer ("t", Var "x");
    Derefer ("s", Var "y");
  ]

let initSymbStore = ["x", S.Bar "x"; "y", S.Bar "y"; "t", S.Hat "t"; "s", S.Hat "s"]

let main () =
  print_string (string_of_prog swap_body);
  print_endline "-----";
  Precond.debug_sdhf [initSymbStore, [], [], []];
  let res = Precond.preProg swap2_body (initSymbStore, [], [], []) in
  res

let _ = main ()
