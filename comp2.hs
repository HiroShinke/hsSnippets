

import Data.Maybe

{-
  from BAHR and Hutton's, Calculating Correct Compilers

  Second approach. two code continuation
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

   exec (comp' x sc fc) s = case eval x of
                               Just n -> exec sc (VAL n:s)
                               Nothing -> exec fc s

   exec (comp' (Val n) sc fc) s
=  exec sc (VAL n:s)
=  exec (PUSH n sc) s

   exec (comp' Throw sc fc) s
=  exec fc s

   exec (comp' (Add x y) sc fc) s
=  case eval x of
        Just n -> case eval y of
                      Just m -> exec sc (VAL (n+m):s)
                      Nothing -> exec fc s
        Nothing -> exec fc s
=  case eval x of
        Just n -> case eval y of
                      Just m -> exec (ADD sc) (VAL m:VAL n: s)
                      Nothing -> exec fc s
        Nothing -> exec fc s
  { define: exec (POP fc) (VAL n:s) = exec fc s )
=  case eval x of
        Just n -> case eval y of
                      Just m -> exec (ADD sc) (VAL m:VAL n: s)
                      Nothing -> exec (POP fc) (VAL n: s)
        Nothing -> exec fc s
=  case eval x of
        Just n -> exec (comp' y (ADD sc) (POP fc)) (VAL n: s)
        Nothing -> exec fc s
=  exec (comp' x (comp' y (ADD sc) (POP fc)) fc ) s


   exec (comp' (Catch x h) sc fc) s
=  case eval x of
      Just n -> exec sc (VAL n: s)
      Nothing -> exec (comp' h sc fc) s
=  exec (comp' x sc (comp' h sc fc) ) s


   exec (comp x) s = case eval x of
                        Just n -> VAL n: s
                        Nothing -> s
   { define : exec HALT s = s )
=  exec (comp x) s = case eval x of
                        Just n -> exec HALT (VAL n: s)
                        Nothing -> exec HALT s
=  exec (comp x HALT HALT) 

--}

data Code = PUSH Int Code
          | ADD Code
          | POP Code
          | HALT
          deriving Show

data Elm = VAL Int
         deriving Show

type Stack = [Elm]

comp :: Expr -> Code
comp x = comp' x HALT HALT

comp' :: Expr -> Code -> Code -> Code
comp' (Val n) sc fc = PUSH n sc
comp' (Add x y) sc fc = comp' x (comp' y (ADD sc) (POP fc)) fc
comp' Throw sc fc = fc
comp' (Catch x h) sc fc = comp' x sc (comp' h sc fc)

exec :: Code -> Stack -> Stack 
exec (PUSH n c) s = exec c (VAL n:s)
exec (ADD c) (VAL m:VAL n:s) = exec c (VAL (n+m):s)
exec (POP c) (VAL m:s)  = exec c s
exec HALT s = s

runCode c = exec c []

test1 = comp (Add (Val 1) (Val 2))

test3 = comp (Catch (Add (Val 1) (Val 2)) (Val 10))

test5 = comp (Catch (Add (Val 1) Throw) (Val 10))

