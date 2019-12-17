type PVar = String
type FunName = String

data Exp = NULL | Var String
-- data SafeComm = Assign PVar Exp | Malloc PVar
-- data UnSafeComm = DerefAssign PVar Exp | Modify Exp Exp | Free Exp
data BExp = Eeq Exp Exp | Ene Exp Exp
data Comm = Assign PVar Exp | Malloc PVar -- Safe Comm
          | DerefAssign PVar Exp | Modify Exp Exp | Free Exp -- Unsafe Comm
          | Call FunName [Exp] | If BExp Prog Prog
type Prog = [Comm]

data FunDef = Fun FunName [PVar] Prog
