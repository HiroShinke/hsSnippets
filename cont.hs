
import Data.Char

-- Notes about Continuation monad

newtype Cont r a = C ( (a -> r) -> r )

runCont :: Cont r a -> ( (a -> r) -> r )
runCont (C c) = c

instance Functor (Cont r) where
  -- fmap :: (a -> b) -> (Cont r a) -> (Cont r b)
  -- c :: (a -> r) -> r
  -- f :: a -> b
  -- k :: b -> r
  -- k . f :: a -> r
  fmap f (C cx) = C (\k -> cx (k . f) )

instance Applicative (Cont r) where
  -- pure :: a -> Cont r a
  pure a = C (\k -> k a)
  -- (<*>) :: Cont r f -> Cont r a -> Cont r b
  -- k  :: b -> r
  -- fx :: ((a -> b) -> r) -> r
  -- cx :: (a -> r) -> r
  -- f  :: a -> b
  (C fx) <*> (C cx) = C (\k -> fx ( \f -> cx (k . f)) )

{-
  (C fx) <*> (C cx) = C (\k -> (\z -> fx ( \f -> cx (z))) (k .f)

  (C fx) <*> (C cx) = C (\k -> cx' (k .f) )
    where cx' z = fx ( \f -> cx (z) )

  (C fx) <*> (C cx) = fmap f (C cx')
    where cx' z = fx ( \f -> cx (z) )
-}

instance Monad (Cont r) where
  -- (>>=) ::  Cont r a -> (a -> (Cont r b)) -> Cont r b
  -- k :: b -> r
  -- f :: a -> Cont r b
  -- cx :: (a -> r) -> r
  (C cx) >>= f = C (\k -> cx (\a -> (runCont (f a)) k ))


callCC :: ( (a -> Cont r b) -> Cont r a) -> Cont r a
-- k ::  a -> r
-- C (\_ -> k a) :: Cont r b
-- (\a -> C (\_ -> k a)) :: a -> Cont r b
-- fn (\a -> C (\_ -> k a)) :: Cont r a
-- (runCont $ fn (\a -> C (\_ -> k a)) ) k :: r
callCC fn = C $ \k -> (runCont $ fn (\a -> C (\_ -> k a))) k


perm :: [Int] -> Int
perm ns = (runCont (callCC $ \k -> perm' k ns )) id
  where
    -- k :: (a -> Cont r b)
    perm' k [] = return 1
    perm' k (n:ns) = if n == 0
                     then
                       k 0
                     else
                       do
                         prod <- perm' k ns
                         return $ n*prod

perm2 :: [Int] -> String
perm2 ns = (runCont (callCC $ \k -> perm' k ns )) id
  where
    perm' k [] = return ""
    perm' k (n:ns) = if n == 0
                     then
                       do
                         x <- k "0"
                         return x
                     else
                       do
                         prod <- perm' k ns
                         return $ (show n) ++ "*" ++ prod


twoC :: Cont r Int
twoC = return 2
helloC :: Cont r String
helloC = return "hello"

oneC :: Cont r String
oneC = return "1"

--- use as (Cont Int String)
testOneC  = runCont oneC readInt
readInt :: String -> Int
readInt str = read str

testOneC' = do
  one <- oneC             --- Cont r String
  two <- C $ (\k -> k 2)  --- Cont r Int , k :: Int -> r
  return $ one ++ (show two) -- Cont r String

-- +-----------------------+
-- +  Cont a r             |
-- +   +-------------------+
-- +   | \a
-- +   |  +----------------+
-- +   |  | Cont b r       |
-- +   |  |  +-------------+
-- +   |  |  |\b
-- +   |  |  |  +----------+
-- +   |  |  |  +  b ->r   +
-- +---+  +--+  +----------+


twoHelloC = do
    two <- twoC
    hello <- helloC
    return $ (show two)++hello

twoHelloC' = twoC >>= \two ->
                helloC >>= \hello ->
                  return $ (show two)++hello

testCont' = (runCont twoHelloC') id

twoMultiC = do
    two <- twoC
    hello <- C $ \out -> out "hello" ++ out "hello"
    return $ (show two)++hello

testCont''  = runCont twoMultiC id
testCont''' = runCont twoMultiC (++"BOOM!")

multiMultiC = do
    n <- C $ \out -> out "1" ++ out "2"
    l <- C $ \out -> out "a" ++ out "b"
    x <- C $ \out -> out "X" ++ out "Y"
    return $ n++l++x++" "

--- ContT

newtype ContT r m a = CT ( (a -> m r) -> m r )

runContT :: ContT r m a -> ( (a -> m r) -> m r )
runContT (CT c) = c

instance Monad m => Functor (ContT r m) where
  -- fmap :: (a -> b) -> (ContT r m a) -> (ContT r m b)
  fmap f (CT cx) = CT (\k -> cx (k . f) )

instance Monad m => Applicative (ContT r m) where
  -- pure :: a -> ContT r m a
  pure a = CT (\k -> k a)
  -- (<*>) :: ContT r m f -> ContT r m a -> ContT r m b
  (CT fx) <*> (CT cx) = CT (\k -> fx ( \f -> cx (k . f)) )


instance Monad m => Monad (ContT r m) where
  -- (>>=) ::  ContT r m a -> (a -> (ContT r m b)) -> ContT r m b
  (CT cx) >>= f = CT (\k -> cx (\a -> (runContT (f a)) k ))

class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

instance MonadTrans (ContT r) where
  -- lift :: (Monad m) => m a -> t m a
  lift ma = CT (\k -> do a <- ma
                         k a
               )

callCCT :: ( (a -> ContT r m b) -> ContT r m a) -> ContT r m a
callCCT fn = CT $ \k -> (runContT $ fn (\a -> CT (\_ -> k a))) k

--- ContT + Maybe

perm3 :: [Int] -> Maybe String
perm3 ns = (runContT (callCCT $ \k -> perm' k ns )) Just
  where
    perm' k [] = return ""
    perm' k (n:ns) = if n == 0
                     then
                       lift Nothing
                     else
                       do
                         prod <- perm' k ns
                         return $ (show n) ++ "*" ++ prod

helloCT :: ContT r Maybe String
helloCT = return "hello"

nothingCT :: ContT r Maybe String
nothingCT = lift Nothing

doHelloCT = do
  h1 <- helloCT   -- ContT String Maybe String
                  -- == CT ((String -> Maybe String) -> String)
  h2 <- helloCT   -- ContT String Maybe String
  h3 <- helloCT   -- ContT String Maybe String
  return $ h1 ++ h2++ h3

doHelloCT' = do
  h1 <- helloCT
  h2 <- nothingCT
  h3 <- helloCT
  return $ h1 ++ h2++ h3


-- +-----------------------+
-- +  ContT a Maybe r      |
-- +   +-------------------+
-- +   | \ a -- bind in inner monad
-- +   |  +----------------+
-- +   |  | ContT b Maybe r|
-- +   |  |  +-------------+
-- +   |  |  |\ b -- bind in inner monad
-- +   |  |  |  +----------+
-- +   |  |  |  +  b -> m r+
-- +---+  +--+  +----------+


testHelloCT1 = runContT doHelloCT  return
testHelloCT2 = runContT doHelloCT' return

--- ContT + Identity

newtype Identity a = I a

runIdentity (I a) = a

instance Show a => Show (Identity a) where
  show (I a) = show a

instance Functor Identity where
  fmap f (I a) = I (f a)
  
instance Applicative Identity where
  pure a = I a
  (I f) <*> (I a) = I (f a)
  
instance Monad Identity where
  (I a) >>= f = f a

helloCT' :: ContT r Identity String
helloCT' = return "hello"

doHelloCT3 = do
  h1 <- helloCT'
  h2 <- helloCT'
  h3 <- helloCT'
  return $ h1 ++ h2++ h3

testHelloCT3 = runContT doHelloCT3 I

-------

type Cont' r a = ContT r Identity a

doHelloCT6 = do
  h1 <- helloCT
  h2 <- helloCT
  h3 <- helloCT
  return $ h1 ++ h2++ h3
  where
    helloCT :: Cont' r String
    helloCT = return "hello"

testHelloCT6 = runContT doHelloCT6 return

--- ContT + []

helloCT'' :: ContT r [] String
helloCT'' = lift ["hello!","goobye!","morning!"]

doHelloCT4 = do
  h1 <- helloCT''
  h2 <- helloCT''
  h3 <- helloCT''
  return $ h1 ++ h2++ h3

doHelloCT5 = do
  h1 <- helloCT''
  h2 <- lift []
  h3 <- helloCT''
  return $ h1 ++ h2++ h3

testHelloCT5 = runContT doHelloCT5 return

