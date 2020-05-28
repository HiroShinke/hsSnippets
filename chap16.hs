

-- excersize 1.

{-

 Say
 add Zero m = m
 add (Succ n) m = Succ (add n m)

 probe 
 add n (Succ m) = Succ (add n m).

 1) base

  add Zero (Succ m) 
  (add)
= Succ m
  (add inverse)
= Succ (add Zero m)

  2) recursion

hypothesis for n
  add n (Succ m) = Succ (add n m)

  add (Succ n) (Succ m)
  (add)
= Succ (add n (Succ m))
  (hypothesis)
= Succ (Succ (add n m))
  (add inverse)
= Succ (add (Succ n) m)

-}

-- exercize 2.

{-
   probe.
   add n m = add m n

   The follwing may be assumed
   add n Zero = n (*)

1) base
   add Zero m = 
   (add)
=  m
   (*)
=  add m Zero

2) induction

assume
   add n m = add m n

   add (Succ n) m
   (add)
=  Succ (add n m)
   (hypothesis)
=  Succ (add m n)
   (exercise 1)
=  add  m (Succ n)

-}


-- exercise 3.

{-
  assume
  all p [] = True
  all p (x:xs) = p x && all p xs

  Probe
  all (== x) (replicate n x) = True.

1) base

  all (== x) (replicaxte 0 x)
= all (== x) []
= True

2) induction

  all (== x) (replicate (n+1) x)
= all (== x) (x : replicate n x)
= x == x && all (== x) (replicate n x)
= x == x && True
= True

-}


-- exercise 4.

{-

  definision

  [] ++ ys = ys                   ---- (d1)
  (x:xs) ++ ys = x : (xs ++ ys )  ---- (d2)

  probe

  xs ++ [] = []
  xs ++ (ys + zs) = (xs ++ ys) ++ zs

1-1) base

  [] ++ [] 
  (d1)
= []

1-2) induction

  (x:xs) ++ []
  (d2)
=  x : (xs ++ [])
  (hypothesis)
=  x : xs

2-1) base

  [] ++ (ys ++ zs)
= ys ++ zs
= ([] ++ ys) ++ zs

2-2) induction

  (x:xs) ++ (ys ++ zs)
  (d2)
= x: (xs ++ (ys ++ zs))
  (hypothesis)
= x: ((xs ++ ys) ++ zs)
  (d2 inverse)
= (x : (xs ++ ys)) ++ zs
  (d2 inverse)
= ((x:xs) ++ ys) ++ zs


{-

  Exercise 5

  take 0 _  = []
  take _ [] = []
  take n (x:xs) = x : take (n-1) xs

  drop 0 xs = xs
  drop _ [] = []
  drop n (_:xs) = drop (n-1) xs


  prove

  take n xs ++ drop n xs = xs

1) base(1)

  take n [] ++ drop n []
= [] ++ []
= []

   base(2)

  take 0 xs ++ drop n xs
= [] ++ xs
= xs

2) induction

  take n (x:xs) ++ drop n (x:xs)
= x : take (n-1) xs ++  drop (n-1) xs
= x : (take (n-1) xs ++ drop (n-1) xs)
= x : xs

-}

{-

  exercise 6

  data Tree = Leaf Int | Node Tree Tree

  leafCount (Leaf n)   = 1
  leafCount (Node l r) = leafCount l + leafCount r

  nodeCount (Leaf n)   = 0
  nodeCount (Node l r) = 1 + nodeCount l + nodeCount r

  probe.

  leafCount = nodeCount + 1

1) base

  leafCount (Leaf n) = 1
  nodeCount (Leaf n) = 0

2) induction

  leafCount (Node l r)
= leafCount l + leafCount r
= nodeCount l + 1 + nodeCount r + 1
= 1 + nodeCount (Node l r)

-}


{-
  Exercise 7

  fmap id = id                   ---(1)
  fmap (g . h) = fmap g . fmap h ---(2)


  fmap f Nothing  = Nothing    ---(fmap)
  fmap f (Just a) = Just (f a) ---(fmap)

(1)
  fmap id Nothig
  (fmap)
= Nothing
  (id)
= id Nothing

  fmap id (Just a)
  (fmap)
= (Just (id a))
= Just a
= id (Just a)

(2)
  fmap (g . h) Nothing
= Nothing
= fmap g Nothing
= fmap g (fmap h Nothing)
= (fmap g . fmap h ) Nothing

  fmap (g . h) (Just a)
= Just ((g . h) a)
= Just (g (h a))
= fmap g (Just (h a))
= fmap g (fmap h (Just a))
= (fmap g . fmap h ) (Just a)


 Exercise 8

  data Tree a = Leaf a | Node (Tree a) (Tree a)

  instance Functor Tree where
  fmap g (Leaf a) = Leaf (g a)
  fmap g (Node l r) = Node (fmap g l) (fmap g l)


(1)
  fmap id (Leaf a)
= Leaf (id a)
= Leaf a
= id (Leaf a)

  fmap id (Node l r)
= Node (fmap id l) (fmap id r)
= Node (id l) (id r)
= Node l r
= id (Node l r)

(2)
  fmap (g . h) (Leaf a)
= Leaf ((g . h) a)
= Leaf (g (h a))
= fmap g (Leaf (h a))
= fmap g (fmap h (Leaf a))
= (fmap g . fmap h) (Leaf a)

  fmap (g . h) (Node l r)
= Node (fmap (g . h) l) (fmap (g . h) r)
= Node ((fmap g . fmap h) l) ((fmap g . fmap h) r)
= Node (fmap g (fmap h l)) (fmap g (fmap h r))
= fmap g (Node (fmap h l) (fmap h r))
= fmap g (fmap h (Node l r))
= (fmap g. fmap h) (Node l r)


 Exercise 9

pure id <*> x = x                                  -- (a1)
pure (g x)    =  pure g <*> pure x                 -- (a2)
x <*> pure y  =  pure (\g -> g y) <*> x            -- (a3)
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*z>      -- (a4)

instance Applicative Maybe where
-- pure :: a -> Maybe a
pure a = Just a
-- <*> ::  Maybe (a -> b) -> Maybe a -> Maybe b
Nothing <*> _ = Nothing
_ <*> Nothing = Nothing
(Just f) <*> (Just a) = Just (f a)

(a1)
   pure id <*> Nothing
=  Nothing

   pure id <*> Just a
=  Just (id a)
=  Just a

(a2)

   pure (g x)
=  Just (g x)
=  Just g <*> Just x
=  pure g <*> pure x


(a3)
   trivial if nothing

   Just f <*> pure y
=  Just f <*> Just y
=  Just (f y)
=  Just ((\g -> g y) f)
=  Just (\g -> g y) <*> Just f
=  pure (\g -> g y) <*> Just f

(a4)
   trivial if any variable is Nothing

   (Just f) <*> ((Just g) <*> (Just a))
=  Just f <*> Just (g a)
=  Just (f (g a) )
=  Just ((f . g) a)
=  Just (f . g) <*> Just a
=  (Just (f.) <*> Just g ) <*> Just a
=  (Just (.)  <*> Just f <*> Just g ) <*> Just a


 Exercise 10

 monad

 return x >>= f = f x   ----- (m1)
 mx >>= return  = mx    ------(m2)
 (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g) ) --(m3)


instance Monad [] where
return x = [x]
mx >>= f = [ f x | x <- mx ]


(m1)

   return x >>= f
=  [x] >>= f
=  [y | x <- [x], y <- f x ]
=  f x

(m2)

   mx >>= return
=  [y | x <- mx, y <- return x]
=  [y | x <- mx, y <- [x] ]
=  [x | x <- mx ]
=  mx

(m3)

  (mx >>= f) >>= g
= [y | x <- mx, y <- f x] >>= g
= [z | x <- mx, y <- f x, z <- g y ]
= [z | x <- mx, z <- [z | y <- f x, z <- g y] ]
= [z | x <- mx, z <- (f x >>= g )  ]
= mx >>= (\x -> f x >> g)

-}
-}

-- Exercise 11

-- comp' e c = comp e ++ c

type Stack = [Int]
type Code  = [Op]
data Op    = PUSH Int | ADD
  deriving Show

data Expr = Val Int | Add Expr Expr

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD:c) )

{-

  comp' (Val n) c
= comp  (Val n) ++ c
= [PUSH n] ++ c
= PUSH n : c

= comp' (Add x y) c
= comp (Add x y) ++ c
= comp x ++ comp y ++ [ADD] ++ c
= comp x ++ comp y ++ (ADD : c)
= comp x ++ 
= comp' x (comp' y (ADD:c) )

-}

