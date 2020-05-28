

import Prelude hiding (fail)
import Data.Maybe

{-
  from BAHR and Hutton's, Calculating Correct Compilers

  First approach. one code continuation
-} 

data Expr = Val Int
          | Add Expr Expr
          | Throw
          | Catch Expr Expr

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Add x y) = case eval x of
                   Just n -> case eval y of
                     Just m -> Just (n + m)
                     Nothing -> Nothing
                   Nothing -> Nothing
eval Throw     = Nothing
eval (Catch x h) = case eval x of
                     Just n -> Just n
                     Nothing -> eval h


{--
   exec (comp e) s = eval e : s
   exec (comp' e c) s = exec c (eval e : s)

exec (comp' x c) s = case eval x of
                      Just n -> exec c (VAL n:s)
                      Nothing -> fail s

  exec (comp' (Val n) c) s
= exec c (VAL n:s)
= exec (PUSH n c) s

  exec (comp' Throw c) s
= fail s
= exec FAIL s

  exec (comp' (Add x y) c) s
= case eval x of
     Just n -> case eval y of
                 Just m -> exec (ADD c) (VAL m: VAL n: s)
                 Nothing -> fail s
     Nothing -> fail s
  { define : fail (VAL n: s) = fail s }
= case eval x of
     Just n -> case eval y of
                 Just m -> exec (ADD c) (VAL m: VAL n: s)
                 Nothing -> fail (VAL n: s)
     Nothing -> fail s
= case eval x of
     Just n -> exec (comp' y (ADD c)) (VAL n: s)
     Nothing -> fail s
= exec (comp' x (comp' y (ADD c))) s

  exec (comp' (Catch x h) c) s
  { specification }
= case eval x of
       Just m -> exec c (VAL m: s)
       Nothing -> exec (comp' h c) s
  { define: exec (comp' h c) s = fail s 
    fail (HAN c':s) = exec (comp' h c) s }
= case eval x of
       Just m -> exec (UNMARK c) (VAL m: HAN c': s)
       Nothing -> fail (HAN c':s)
= exec (comp' x (UNMARK c)) (HAN c': s)
   { define : exec (MARK c' c) s = exec c (HAN c': s) )

= exec (MARK (comp' h c) (comp' x (UNMARK c))) s

  exec (comp x) s = case eval x of
                       Just n -> VAL n : s
                       Nothing -> fail s
 { define : exec HALT s = s }
= case eval x of
     Just n -> exec HALT (VAL n : s)
     Nothing -> fail s

= exec (comp' x HALT) s


--}

data Code = PUSH Int Code
          | FAIL
          | ADD Code
          | MARK Code Code
          | UNMARK Code
          | HALT
          deriving Show

data Elm = VAL Int
         | HAN Code
         deriving Show

type Stack = [Elm]

comp :: Expr -> Code
comp x = comp' x HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = (comp' x (comp' y (ADD c)))
comp' Throw c = FAIL
comp' (Catch x h) c = MARK (comp' h c) (comp' x (UNMARK c))

exec :: Code -> Stack -> Stack 
exec (PUSH n c) s = exec c (VAL n:s)
exec FAIL s = fail s
exec (ADD c) (VAL m:VAL n:s) = exec c (VAL (n+m):s)
exec (MARK c' c) s = exec c (HAN c':s )
exec (UNMARK c) (VAL m: HAN _: s) = exec c (VAL m: s)
exec HALT s = s

fail :: Stack -> Stack
fail []         = []
fail (VAL n: s) = fail s
fail (HAN c':s) = exec c' s

runCode c = exec c []

test1 = comp (Add (Val 1) (Val 2))

test3 = comp (Catch (Add (Val 1) (Val 2)) (Val 10))

test5 = comp (Catch (Add (Val 1) Throw) (Val 10))

