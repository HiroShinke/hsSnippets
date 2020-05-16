{-# LANGUAGE FlexibleContexts,FlexibleInstances,MultiParamTypeClasses,UndecidableInstances #-}


import Data.Char
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader

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


--- ContT + Writer

instance (Monoid w, MonadWriter w m) => MonadWriter w (ContT r m) where
  -- tell :: w -> ContT r m ()
  -- k :: () -> m r
  tell w = CT (\k -> tell w >>= k )
  -- pass   :: ContT r m (a,w -> w) -> ContT r m a 
  -- fx :: ((a,w->w) -> r m) -> r m
  -- aww :: (a,w->w)
  -- pass :: m (a,w -> w) -> m a
  -- k :: a -> m r
  pass (CT fx) = CT (\k -> fx (\aww -> do
                                  a <- pass $ return aww
                                  k a)
                    )
  -- listen :: ContT r m a -> ContT r m (a,w) 
  -- ContT r m (a,w) :: ((a,w) -> m r) -> m r
  -- k :: (a,w) -> m r
  -- cx :: (a -> m r) -> m r
  -- listen :: m a -> m (a,w)
  listen (CT cx) = CT (\k -> cx (\a -> do
                                    (a,w) <- listen $ return a
                                    k (a,w))
                      )



  
perm4 :: [Int] -> ContT r (Writer [String])  Int
perm4 ns = (runContT (callCCT $ \k -> perm' k ns )) return
  where
    perm' k [] = do
      lift $ tell ["and at last we get "]
      return 1
    perm' k (n:ns) = if n == 0
                     then
                       do
                         lift $ tell ["abort!! " ++ (show n) ]
                         k 0
                     else
                       do
                         lift $ tell ["continue!! " ++ (show n)]
                         prod <- perm' k ns
                         return $ n * prod


--- ContT + Reader

{-
instance MonadReader e (Reader e) where
    -- ask :: Reader e e 
    ask       = R id
    -- local :: (e -> e) -> Reader e a -> Reader e a
    local f c = R $ \e -> runReader c (f e)
-}

instance (MonadReader e m) => MonadReader e (ContT r m) where
  -- ask :: ContT r (Reader e) e
  -- k ::  e -> Reader e r
  ask = CT (\k -> ask >>= k )
  -- local :: (e -> e) -> ContT r (Reader e) a -> ContT r (Reader e) a
  -- k ::  a -> Reader e r
  -- cx :: (a -> Reader e r) -> Reader e r
  local f (CT cx) = CT (\k -> local f $ cx ( \a -> k a ) )

-- simple implementation of environment

type Env = [(String,String)]  
  
env1 = [("sato","yokohama"),
        ("tanaka","tokyo"),
        ("sakai", "osaka")]

addEnvs :: [(String,String)] -> Env -> Env
addEnvs kv env = kv ++ env
       
readerFunc :: String -> Env -> String
readerFunc s e = case lookup s e of
                   Just p -> p
                   Nothing -> "nowhere"


perm5 :: [String] -> ContT r (Reader Env) [String]
perm5 ns = (runContT (callCCT $ \k -> perm k ns )) return
  where
    perm  k ns = local (addEnvs [("sato","sapporo")] ) (perm' k ns) 
    perm' k [] = do
      return []
    perm' k (n:ns) = if n == ""
                     then
                       k []
                     else
                       do
                         a <- asks $ readerFunc n
                         as <- perm' k ns
                         return $ a:as

testContReader1 = runReader ((runContT $ perm5 ["sato","sakai"] ) return ) env1
testContReader2 = runReader ((runContT $ perm5 ["sato",""] ) return ) env1

--- ContT + State

{-
instance MonadState (State s) s where 
  -- get :: m s
  get   = State $ \s -> (s,s) 
    -- put :: s -> m ()
  put s = State $ \_ -> ((),s) 
-}

instance MonadState s (ContT r (State s)) where
  -- get :: ContT r (State s)
  get = lift $ get
  put s = lift $ put s


getC str = do
  n <- lift $ get
  let c = str !! n
  lift $ put (n + 1)
  return c
  
contState :: String -> ContT r (State Int) String
contState str = runContT (callCCT $ \k -> func k) return
  where
    func k = do
      a <- getC str
      b <- getC str
      c <- getC str
      return [a,b,c]
  
testContState = runState (runContT (contState "abc") return) 0


contState1 :: String -> ContT r (State Int) String
contState1 str = runContT (callCCT $ \k -> func k) return
  where
    func k = do
      a <- getC str
      b <- getC str
      k "xxx"
      c <- getC str
      return [a,b,c]

testContState1 = runState (runContT (contState1 "abc") return) 0


contState2 :: String -> ContT r (State Int) String
contState2 str = runContT (callCCT $ \k -> func k) return
  where
    func k = do
      a <- CT (\k0 -> do n <- lift $ get
                         x <- k0 'A'
                         lift $ put n
                         y <- k0 'B'
                         lift $ put n
                         z <- k0 'C'
                         return $ x ++ y ++ z
              )
      b <- getC str
      c <- getC str
      return [a,b,c,'x']

testContState2 = runState (runContT (contState2 "abc") return) 0


