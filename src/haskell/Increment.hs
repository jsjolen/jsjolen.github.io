{-# LANGUAGE StandaloneDeriving #-}
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State

data Cont = Cont Int
deriving instance Show Cont

increment :: State Cont Cont
increment = state (\x-> case x of
                      (Cont v) -> (Cont v, Cont (v+1)))
main =
  do
    x <- Cont 0;
    increment x;
    putln x;
    
