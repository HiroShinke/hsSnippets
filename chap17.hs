

data Expr = Val Int | Add Expr Expr
  deriving Show

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

type Stack = [Int]

push :: Int -> Stack -> Stack
push n s = n : s 

add :: Stack -> Stack
add (n:m:ns) = (n + m) : ns

eval' :: Expr -> Stack -> Stack
-- eval' e s = eval e : s
eval' (Val n) s = push n s
eval' (Add x y) s = add (eval' y (eval' x s))


eval2 :: Expr -> Stack
eval2 e = eval' e []

type Cont = Stack -> Stack

eval'' :: Expr -> Cont -> Cont

-- eval'' e c s = c (eval' e s)

eval'' (Val n) c s = c (push n s)
eval'' (Add x y) c s = eval'' x (eval'' y (c . add)) s

{-
  eval'' (Add x y) c s
= c (add (eval' y (eval' x s)))
= (c .add) (eval' y (eval' x s))
= eval'' y  (c . add) (eval' x s)
= eval'' x (eval'' y (c . add) ) s

-}

eval3 :: Expr -> Cont
eval3 e s = eval'' e id s


haltC :: Cont
haltC = id

pushC :: Int -> Cont -> Cont
pushC n c = c . push n

addC :: Cont -> Cont
addC c = c . add

eval''' :: Expr -> Cont -> Cont
eval''' (Val n) c = pushC n c
eval''' (Add x y) c = eval''' x (eval''' y (addC c))


data Code = HALT | PUSH Int Code | ADD Code
  deriving Show

exec :: Code -> Cont
exec HALT = haltC
exec (PUSH n c) = pushC n (exec c)
exec (ADD c) = addC (exec c)

{-

  exec HALT s
= haltC s
= id s
= s

  exec (PUSH n c) s
= pushC n (exec c) s
= (exec c . push n) s
= exec c (push n s)
= exec c (n : s)

  exec (ADD c) s
= addC (exec c) s
= (exec c . add) s
=  exec c (add s)
=  exec c (n + m : s')

  exec HALT s = s
  exec (PUSH n c) s = exec c (n : s)
  exec (ADD c) (n : m : s) = exec c (n+m :s)

-}

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))

{-

   exec (comp e) s = eval e : s
   exec (comp' e c) s = exec c (eval e : s)

  exec (comp' (Val n) c) s
= exec c (eval (Val n) : s)
= exec c (n : s)
= exec c' s 
= exec (PUSH n c) s

  exec (comp' (Add x y) c) s
= exec c (eval (Add x y) : s)
= exec c (eval x + eval y: s)
= exec c' (eval x : eval y : s )
= exec (ADD c) (eval y : eval x : s )
= exec (comp' y (ADD c)) (eval x : s)
= exec (comp' y (comp' x (ADD c))) s

  exec (comp' x c) s = exec c (eval x : s)
  exec (comp' y c) s = exec c (eval y : s)


  exec (comp e) s
= eval e : s
= exec HALT (eval e : s)
= exec (comp' e HALT) s

-}


