type PVar = String
data Term = Nil | Bar String | Hat String
data Pred = EQ Term Term | NE Term Term
type PathExp = [Pred]
type SymbStore = [(PVar, Term)]
type PureFml = (SymbStore, PathExp)
data SpatPred = Mapsto Term Term | Ls Term Term
type SpatFml = [SpatPred]
type SymbHeap = (PureFml, SpatFml)

--type PVar = String
type FunName = String

data Exp = NULL | Var String
-- data SafeComm = Assign PVar Exp | Malloc PVar
-- data UnSafeComm = DerefAssign PVar Exp | Modify Exp Exp | Free Exp
data BExp = Eeq Exp Exp | Ene Exp Exp
data Comm = Assign PVar Exp | Malloc PVar -- Safe Comm
          | Derefer PVar Exp | Modify Exp Exp | Free Exp -- Unsafe Comm
          | Call FunName [Exp] | If BExp Prog Prog
type Prog = [Comm]

data FunDef = Fun FunName [PVar] Prog

apply :: SymbStore -> Exp -> Term
apply s NULL = Nil
apply s (Var x) = maybe undefined $ lookup x s

pre :: Comm -> (SymbStore, PathExp, SpatFml, SpatFml) -> [(SymbStore, PathExp, SpatFml, SpatFml)]
pre (Assign x e) (s, d, heap, foot) = return (((x, e'):s), d, heap, foot) where
  e' = apply s e
pre (Malloc x) (s, d, heap, foot) = return ((x, Hat a):s, (Hat a `NE` Nil):d, (Hat a `Mapsto` Hat a'):heap, foot)
pre (Derefer x e) (s, d, heap, foot) | allocated(d, heap, e) = undefined
pre (Derefer x e) (s, d, heap, foot) = undefined
pre (Modify e e') (s, d, heap, foot) | allocated(d, heap, e) = undefined
pre (Modify e e') (s, d, heap, foot) = undefined
pre (Free e) (s, d, heap, foot) | allocated(d, heap, e) = undefined
pre (Free e) (s, d, heap, foot) = undefined
pre (Call f es) (s, d, heap, foot) = undefined
pre (If b p1 p2) (s, d, heap, foot) = undefined

preProg :: Prog -> (SymbStore, PathExp, SpatFml, SpatFml) -> [(SymbStore, PathExp, SpatFml, SpatFml)]
preProg [] a = a
preProg (c:p) (s, d, h, f) = do
  (s', d', h', f') <- pre c (s, d, h, f)
  preProg p (s', d', h', f')

main = print undefined
