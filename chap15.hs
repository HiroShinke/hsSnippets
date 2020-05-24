

-- exersice 4.

fibs :: [Integer]
fibs = 0:1:fibs'
  where
    fibs' = zipWith (+) fibs (tail fibs)
    
-- exercise 5.

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

repeatT :: a -> Tree a
repeatT a = as
  where 
    as = Node as a as

takeT :: Int -> Tree a -> Tree a
takeT 0 _     = Leaf
takeT _ Leaf  = Leaf
takeT n (Node lb a rb) = (Node (takeT (n-1) lb) a (takeT (n-1) rb) )

replicateT n  = takeT n . repeatT

-- exercise 6.

sqroot :: Double -> Double
sqroot n = snd $ head seq
  where
    values = iterate next 1.0
    next a = (a + n/a) /2
    zipv   = zip (tail values) values
    seq    = dropWhile (\(a,b) -> abs(a-b) > 0.000001) zipv
