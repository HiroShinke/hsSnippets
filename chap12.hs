
-- Grahum Hutton's Chapter 12

import Data.Char
import System.IO.Unsafe

trace :: Show a => String -> a -> a 
trace s x = unsafePerformIO ( do
                                putStrLn s
                                putStrLn $ "result " ++ show x
                                return x
                            )
-- trace _ x = x


data Mayb a = Nothin | Jus a
  deriving (Show,Eq,Ord)

instance Functor Mayb where
  fmap _ Nothin = Nothin
  fmap f (Jus a) = Jus (f a)

instance Applicative Mayb where
  pure a = Jus a
  (Jus f) <*>  mx = fmap f mx
  Nothin  <*>  mx = Nothin


fmap0 :: Applicative f => a -> f a
fmap0 = pure

fmap1 :: Applicative f => (a -> b) -> f a -> f b
fmap1 g x = pure g <*> x

fmap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2 g x y = pure g <*> x <*> y

sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' []     =  pure []
sequenceA' (x:xs) =  pure (:) <*> x <*> sequenceA' xs

test1 = sequenceA' [replicate 10 (Jus 4)]
test2 = sequenceA' [[1]]

getChars :: Int -> IO String
getChars n = sequenceA' (replicate n getChar)

test3 = (+) <$> (Jus 1) <*> (Jus 2)
test4 = pure (+) <*> (Jus 1) <*> (Jus 2)

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv n m = Just (n `div` m)

data Expr = Val Int | Div Expr Expr

{-
-- invalid declaration

eval1 :: Expr -> Maybe Int
eval1 (Val n)   = pure n
eval1 (Div x y) = pure safeDiv <*> eval1 x <*> eval1 y
-}


newtype ST s a = S (s -> (a,s))

app :: ST s a -> s -> (a,s)
app (S st) x = st x

instance Functor (ST s) where
  -- (a -> b) -> ST s a -> ST s b
  fmap g st = S (\s -> let (a,s') = app st s in (g a,s'))
                       
instance Applicative (ST s) where
  -- a -> ST s a
  pure a = S (\s -> (a,s))
  -- ST s (a -> b) -> ST s a -> ST s b
  mf  <*> mx = S (\s -> let (f,s')  = app mf s
                        in app (fmap f mx) s' )
{-
  -- another implementation 
  mf  <*> mx = S (\s -> let (f,s')  = app mf s
                            (x,s'') = app mx s'
                        in (f x,s'')
                 )
-}             
                          
instance Monad (ST s) where
  -- (>>=) :: ST s a -> (a -> ST s b) -> (a -> ST s b)
  st >>= f = S (\s -> let (a,s') = app st s
                      in  app (f a) s' )
{-
  -- another implementation 
  st >>= f = S (\s   -> let (st',s') = app (fmap f st) s
                        in app st' s' )
-}
    
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show


rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _)   n = (Leaf n, n + 1)
rlabel (Node l r) n = let (l',n')  = rlabel l n
                          (r',n'') = rlabel r n'
                      in (Node l' r',n'')

sample1 = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')
testRlabel = rlabel sample1 0


fresh :: ST Int Int
fresh = S (\n -> (n,n+1))

-- monadic

mlabel :: Tree a -> ST Int (Tree Int)
mlabel (Leaf _)   = do n <- fresh
                       return $ Leaf n
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return $ Node l' r'
                       
testMlabel = app (mlabel sample1) 0

-- applicative style

alabel :: Tree a -> ST Int (Tree Int)
alabel (Leaf _)   = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r


testAlabel = app (alabel sample1) 0
  
----

conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing


filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p []     = return []
filterM p (x:xs) = do b <- p x
                      ys <- filterM p xs
                      return $ if b then (x:ys) else ys
                      
testFilterM = filterM (const [True,False]) [1,2,3]

                        
                     
join :: Monad m => m (m a) -> m a
join mmx = do mx <- mmx
              x <- mx
              return x

-- Exercise 1

data Tree2 a = Leaf2 | Node2 (Tree2 a) a (Tree2 a)
  deriving Show

instance Functor Tree2 where
  fmap f Leaf2 = Leaf2
  fmap f (Node2 l a r) = Node2 (fmap f l) (f a) (fmap f r)

-- Exercise 2

{-

-- duplicate (defined in GHC.Base)

instance Functor ((->) r) where
  -- fmap :: (a -> b) -> (r -> a) -> (r -> b)
  fmap f g = f . g

-}

newtype Func a b = F (a -> b)

instance Functor (Func r) where
  -- fmap :: (a -> b) -> F (r -> a) -> F (r -> b)
  fmap f (F g) = F (f . g)

  
-- Exercise 3

{-

instance Applicative ((->) r) where
  -- pure :: a -> (r -> a)
  pure a = const a
  -- <*> :: (r -> (a -> b)) -> (r -> a) -> (r -> b)
  fx <*> fa = (\r -> (fx r) (fa r) )

  -- <*> :: (r -> a -> b) -> (r -> a) -> (r -> b)
  -- fx <*> fa = (\r -> fx r (fa r) )

-}    

instance Applicative (Func r) where
  -- pure :: a -> F (r -> a)
  pure a = F (const a)
  -- <*> :: (r -> (a -> b)) -> (r -> a) -> (r -> b)
  F fx <*> F fa = F (\r -> (fx r) (fa r) )


-- Exercise 4

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  fmap f (Z [])     = Z []
  fmap f (Z (x:xs)) = Z (fmap f xs)

instance Applicative ZipList where
  pure a = Z (repeat a)
  (Z fs) <*> (Z xs) = Z (zipWith ($) fs xs)
  
-- Exercise 6 

{-
instance Monad ((->) r) where
  -- (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
  mx >>=  f = (\r -> (f (mx r)) r)
-}

instance Monad (Func r) where
  -- (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
  F mx >>= f = F (\r -> let (F g) = f (mx r) in g r)

-- Exercise 7

data Expr2 a = Var2 a | Val2 Int | Add2 (Expr2 a) (Expr2 a)
  deriving Show

instance Functor Expr2 where
  -- fmap :: (a -> b) -> Expr2 a -> Expr2 b
  fmap f (Var2 a) = Var2 (f a)
  fmap f (Val2 n) = Val2 n
  fmap f (Add2 l r) = Add2 (fmap f l) (fmap f r)

instance Applicative Expr2 where
  -- pure :: a -> Expr2 a
  pure a = Var2 a
  -- (<*>) :: (Expr2 (a -> b) ) -> (Expr2 a) -> (Expr2 b)
  (Var2 f) <*> ax = fmap f ax
  (Val2 n) <*> ax = Val2 n
  (Add2 l r) <*> ax = Add2 (l <*> ax) (r <*> ax)
    
instance Monad Expr2 where
  -- (>>=) :: Expr2 a -> (a -> Expr2 b) -> Expr2 b
  (Var2 a) >>= f = f a
  (Val2 n) >>=  f = Val2 n
  (Add2 l r) >>= f = Add2 (l >>= f) (r >>= f)

-- >>='s meaning is assignments.

subst0 :: String -> Expr2 String
subst0 "a" = Val2 0
subst0 "b" = Val2 1
subst0 "c" = Val2 2
subst0 "d" = Val2 3
subst0 _ = Val2 10

subst1 :: String -> Expr2 Int
subst1 "a" = Var2 0
subst1 "b" = Var2 1
subst1 "c" = Var2 2
subst1 "d" = Var2 3
subst1 _ = Var2 10

subst2 :: Int -> Expr2 a
subst2 n = Val2 (n*2)

testExpr2_1 = Add2 (Add2 (Var2 "a") (Val2 100)) (Add2 (Var2 "b") (Var2 "c"))

test7_1 = testExpr2_1 >>= subst0

test7_2 = do a <- testExpr2_1
             trace "subst0=" $ subst0 a

test7_3 = testExpr2_1 >>= return

test7_4 = (testExpr2_1 >>= subst1) >>= subst2

test7_4' = testExpr2_1 >>= subst1 >>= subst2

test7_5 = testExpr2_1 >>= ( \x -> (subst1 x >>= subst2) )

  

-- Exercise 8

newtype ST2 s a = S2 (s -> (a,s))

app2 :: ST2 s a -> s -> (a,s)
app2 (S2 st) x = st x

instance Functor (ST2 s) where
  -- fmap :: (a -> b) -> ST2 s a -> ST2 s b
  fmap g st = do a <- st
                 return (g a)

instance Applicative (ST2 s) where
  -- pure :: a -> ST2 s a
  pure a = S2 (\s -> (a,s))
  -- (<*>) :: ST2 s (a -> b) -> ST2 s a -> ST2 s b
  str <*> stx = do r <- str
                   x <- stx
                   return (r x)

instance Monad (ST2 s) where
  -- (>>=) :: ST2 s a -> ( a -> ST2 s b ) -> ST2 s b
  st >>= f = S2 (\s -> let (a,s') = app2 st s
                       in  app2 (f a) s' )
                       
