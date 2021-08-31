import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
-- (reset v) -> v
-- (reset E[(shift k body)]) -> (reset ((λ (k) body) (λ (x) (reset E[x]))))

data Var = Var String

data Expr =
  App Expr Expr
  | Abs Var Expr
  | Print Expr
  | Reset Expr
  | Shift Var Expr
  | Begin [Expr]
  | Const Int
  | Ref Var
