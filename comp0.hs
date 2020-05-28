

import Data.Maybe

-- Exercise 1
-- my own answer

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

  exec (comp' (Val n)) s
= exec c (Just n : s)
= exec c' s
= exec (PUSH n c) s

  exec (comp' (Add x y))
= exec c (eval (Add x y ) : s)
= exec c (eval x <+> eval y : s)
= exec c' (eval y: eval x: s)
= exec (ADDM c) (eval y: eval x: s)
= exec (comp' y (ADDM c)) (eval x: s)
= exec (comp' x (comp' y (ADDM c))) s

exec (ADDM c) (n:m:s) = exec c (n <+> m : s)
(<+>) :: Maybe Int -> Maybe Int -> Maybe Int
n <+> m = pure(+) <*> n <*> m 

  exec (comp' Trow) s
= exec c (eval Throw: s)
= exec c (Nothing : s)
= exec c' s
= exec (THROW c) s

  exec (comp' (Catch x h)) s
= exec c (eval (Catch x h) : s)
= exec c' (eval h : eval x : s)
= exec (CATCH c) (eval h : eval x : s)
= exec (comp' x (comp' h (CATCH c))) s


exec (CATCH c) (n:m:s) = exec c (n <?> m : s)

(<?>) :: Maybe Int -> Maybe Int -> Maybe Int
n <?> m = case n of
           Just x -> Just x
           Nothing -> m

exec (comp' x c) s = exec c (eval x : s)


--}

data Code = HALT
          | PUSH Int Code
          | THROW Code
          | ADDM Code
          | CATCH Code

type Stack = [Maybe Int]

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADDM c))
comp' Throw c    = THROW c
comp' (Catch x h) c = comp' x (comp' h (CATCH c))

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (Just n : s)
exec (ADDM c) (n:m:s) = exec c (n <+> m : s)
exec (THROW c) s = exec c (Nothing : s)
exec (CATCH c) (n:m:s) = exec c (n <?> m: s)

n <+> m = pure(+) <*> n <*> m
n <?> m = case n of
            Just x -> Just x
            Nothing -> m

  
