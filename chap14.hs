
import Data.Foldable

{- 

instance Monoid [a] where
  -- mempty :: [a]
  mempty  = []
  -- mappend :: [a] -> [a] -> [a]
  mappend = (++)

-}

{-
instance Monoid a => Monoid (Maybe a) where
  -- mempty :: Maybe a
  mempty  = Nothing
  -- mappend :: Maybe a -> Maybe a -> Maybe a
  mappend mx my = do x <- mx
                     y <- my
                     return $ x `mappend` y
-}

instance Semigroup Int where
    (<>) = (+)

instance Monoid Int where
  mempty = 0

newtype Sum a = Sum { getSum :: a }
  deriving (Eq,Ord,Show,Read)

instance Num a => Semigroup (Sum a) where
  (<>) (Sum x) (Sum y) = Sum (x + y)

instance Num a => Monoid (Sum a) where
  -- mempty :: Sum a
  mempty = Sum 0


newtype Prod a = Prod { getProd :: a }
  deriving (Eq,Ord,Show,Read)

instance Num a => Semigroup (Prod a) where
  (<>) (Prod x) (Prod y) = Prod (x * y)

instance Num a => Monoid (Prod a) where
  -- mempty :: Prod a
  mempty = Prod 1

{-

class Foldable t where
  fold :: Monoid a => t a -> a
  foldMap :: Monoid b => (a -> b) -> t a -> b
  foldr :: ( a -> b -> b ) -> b -> t a -> b
  foldl :: ( b -> a -> b ) -> b -> t a -> b

instance Foldable [] where
  -- fold
  fold [] = mempty
  fold (x:xs) = x `mappend` fold xs
  -- foldMap
  foldMap f [] = mempty
  foldMap f (x:xs)  = f x `mappend` foldMap f xs
  -- foldr
  foldr _ v [] = v
  foldr f v (x:xs) =  f x (foldr f v xs)
  -- foldl
  foldl _ v [] = v
  foldl f v (x:xs) =  foldl f (f v x) xs
-}

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

instance Foldable Tree where
  foldMap f (Leaf a) = f a
  foldMap f (Node lb rb) = foldMap f lb `mappend` foldMap f rb
  foldr f v (Leaf a)     = f a v
  foldr f v (Node lb rb) = foldr f (foldr f v rb) lb
  foldl f v (Leaf a)     = f v a
  foldl f v (Node lb rb) = foldl f (foldl f v lb) rb

tree1 :: Tree Int
tree1 = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)


foldMap2 :: Monoid m => (a -> m) -> [a]-> m
foldMap2 f = foldr (mappend . f) mempty

foldr2   :: Monoid m => (a -> m -> m) -> m -> [a] -> m
foldr2 f v xs = foldMap ((flip f) mempty) xs `mappend` v

{-  
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-}
{-
instance Traversable [] where
  -- traverse :: (a -> f b) -> [a] -> f [b]
  traverse g []      = pure []
  traverse g (x:xs)  = pure (:) <*> g x <*> traverse g xs
-}

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Traversable Tree where
  -- traverse :: (a -> f b) -> Tree a -> f (Tree b)
  traverse g (Leaf a)    = pure (Leaf) <*> g a
  traverse g (Node lb rb)  = pure (Node) <*> traverse g lb <*> traverse g rb

dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n - 1) else Nothing

-- exercise 1.

newtype Mx a b = Mx { runMx :: (a,b) }

instance (Monoid a, Monoid b) => Semigroup (Mx a b) where
  (Mx (a1,b1)) <> (Mx (a2,b2)) = Mx (a1 <> a2, b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (Mx a b) where
  -- mempty :: (a,b)
  mempty = Mx (mempty,mempty)
  -- mappend :: (a,b) -> (a,b) -> (a,b)

-- exercise 2.

newtype Fx a b = Fx { runFx :: a -> b }

instance (Monoid b) => Semigroup (Fx a b) where
  (Fx f) <> (Fx g) = Fx (\a -> (f a) <> (g a) )

instance (Monoid b) => Monoid (Fx a b) where
  -- mempty :: (a -> b)
  mempty =  Fx (\a -> mempty)

-- exercise 3.

newtype Mb a = Mb { runMb :: Maybe a }

instance Foldable Mb where
  -- fold :: Monoid a => t a -> a
  fold (Mb Nothing)  = mempty
  fold (Mb (Just a)) = a
  -- foldMap :: Monoid b => (a -> b) -> t a -> b
  foldMap f (Mb Nothing)  = mempty
  foldMap f (Mb (Just a)) = f a
  --foldr :: ( a -> b -> b ) -> b -> t a -> b
  foldr _ v (Mb Nothing)  = v
  foldr f v (Mb (Just a)) = f a v
  --foldl :: ( b -> a -> b ) -> b -> t a -> b
  foldl _ v (Mb Nothing)  = v
  foldl f v (Mb (Just a)) = f v a

instance Functor Mb where
  fmap f (Mb Nothing)  = Mb Nothing
  fmap f (Mb (Just a)) = Mb (Just (f a))

instance Traversable Mb where
  -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse g (Mb Nothing)  = pure (Mb Nothing)
  traverse g (Mb (Just a)) = pure (Mb . Just) <*> (g a)

  
-- exercise 4.

data BTree a = BLeaf | BNode (BTree a) a (BTree a)
  deriving Show

instance Foldable BTree where
  --fold :: Monoid a => t a -> a
  fold BLeaf = mempty
  fold (BNode lb a rb) = fold lb `mappend` a `mappend` fold rb 
  --foldMap :: Monoid b => (a -> b) -> t a -> b
  foldMap f BLeaf = mempty
  foldMap f (BNode lb a rb) = foldMap f lb `mappend` f a `mappend` foldMap f rb
  --foldr :: ( a -> b -> b ) -> b -> t a -> b
  foldr f v BLeaf = v
  foldr f v (BNode lb a rb) = foldr f (f a (foldr f v rb)) lb
  --foldl :: ( b -> a -> b ) -> b -> t a -> b
  foldl f v BLeaf = v
  foldl f v (BNode lb a rb) = foldl f (f (foldl f v lb) a) rb

instance  Functor BTree where
  fmap f BLeaf = BLeaf
  fmap f (BNode lb a rb) = BNode (fmap f lb) (f a) (fmap f rb)

instance Traversable BTree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g BLeaf           = pure(BLeaf)
  traverse g (BNode lb a rb) = pure(BNode) <*> traverse g lb <*> g a <*> traverse g rb

-- exercise 5.

filter :: Foldable t => (a -> Bool) -> t a -> [a]
filter p = foldMap (\a -> if p a then [a] else []) 

