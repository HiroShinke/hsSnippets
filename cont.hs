{-# LANGUAGE FlexibleContexts,FlexibleInstances,MultiParamTypeClasses,UndecidableInstances #-}


import Data.Char
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader
import System.IO

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


test1 :: [Int] -> Int
test1 ns = (runCont (callCC $ \k -> test' k ns )) id
  where
    -- k :: (a -> Cont r b)
    test' k [] = return 1
    test' k (n:ns) = if n == 0
                     then
                       k 0
                     else
                       do
                         prod <- test' k ns
                         return $ n*prod

test2 :: [Int] -> String
test2 ns = (runCont (callCC $ \k -> test' k ns )) id
  where
    test' k [] = return ""
    test' k (n:ns) = if n == 0
                     then
                       do
                         x <- k "0"
                         return x
                     else
                       do
                         prod <- test' k ns
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

test3 :: [Int] -> Maybe String
test3 ns = (runContT (callCCT $ \k -> test' k ns )) Just
  where
    test' k [] = return ""
    test' k (n:ns) = if n == 0
                     then
                       lift Nothing
                     else
                       do
                         prod <- test' k ns
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
-- +  ContT r Maybe a      |
-- +   +-------------------+
-- +   | \ a -- bind param in inner monad
-- +   |  +----------------+
-- +   |  | ContT r Maybe b|
-- +   |  |  +-------------+
-- +   |  |  |\ b -- bind param in inner monad
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



  
test4 :: [Int] -> ContT r (Writer [String])  Int
test4 ns = (runContT (callCCT $ \k -> test' k ns )) return
  where
    test' k [] = do
      lift $ tell ["and at last we get "]
      return 1
    test' k (n:ns) = if n == 0
                     then
                       do
                         lift $ tell ["abort!! " ++ (show n) ]
                         k 0
                     else
                       do
                         lift $ tell ["continue!! " ++ (show n)]
                         prod <- test' k ns
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


test5 :: [String] -> Reader Env [String]
test5 ns = (runContT (callCCT $ \k -> test k ns )) return
  where
    test  k ns = local (addEnvs [("sato","sapporo")] ) (test' k ns) 
    test' k [] = return []
    test' k (n:ns) = if n == ""
                     then
                       k []
                     else
                       do
                         a <- asks $ readerFunc n
                         as <- test' k ns
                         return $ a:as

testContReader1 = runReader (test5 ["sato","sakai"] ) env1
testContReader2 = runReader (test5 ["sato",""]) env1

--- ContT + ReaderT + Maybe

test6 :: [String] ->  Reader Env [String]
test6 ns = (runContT (callCCT $ \k -> test k ns )) return
  where
    test  k ns = local (addEnvs [("sato","sapporo")] ) (test' k ns) 
    test' k [] = return []
    test' k (n:ns) = if n == ""
                     then
                       k []
                     else
                       do
                         ma <- asks $ lookup n
                         case ma of
                           Just a -> do
                             as <- test' k ns
                             return $ a:as
                           Nothing -> do
                             as <- test' k ns
                             return as

testContReader3 = runReader (test6 ["sato","sakai"])  env1
testContReader4 = runReader (test6 ["sato","suzuki"]) env1

type CRM r a = ContT r (ReaderT Env Maybe) a

test7' :: CRM r [String]
test7'  = do
  ma <- asks $ lookup "sato"
  mb <- asks $ lookup "sakai"
  a <- lift $ lift $ ma
  b <- lift $ lift $ mb
  return [a,b]

testContReader1' = runReaderT (runContT test7' return) env1
testContReader2' = runReaderT (runContT test7' return) [("sato","yokohama")]

test7'' :: [String] -> CRM r [String]
test7'' ns = test ns
  where
    test  ns = local (addEnvs [("sato","sapporo")] ) (test' ns) 
    test' [] = return []
    test' (n:ns) = do ma <- asks $ lookup n
                      a <-  lift $ lift $ ma
                      as <- test' ns
                      return $ a:as

testContReader1'' = runReaderT (runContT (test7'' ["sato","sakai"]) return) env1
testContReader2'' = runReaderT (runContT (test7'' ["sato","suzuki"]) return) env1


-- callCCT :: ( (a -> CRM r b) -> CRM r a) -> CRM r a
-- callCCT() :: CRM r [String]
-- (runContT callCCT()) :: ( [String] -> ReaderT Maybe [String] ) -> ReaderT Maybe [String]

test7 :: [String] -> ReaderT Env Maybe [String]
test7 ns =  (runContT (callCCT $ \k -> test k ns ) ) return
  where
    -- k :: [String] -> CRM r b
    -- helper :: [String] -> ReaderT Env Maybe [String]
    -- helper ns = return ns
    test :: ([String] -> CRM r b) -> [String] -> CRM r [String]
    test  k ns = local (addEnvs [("sato","sapporo")] ) (test' k ns) 
    test' k [] = return []
    test' k (n:ns) = do ma <- asks $ lookup n
                        a <-  lift $ lift $ ma
                        as <- test' k ns
                        return $ a:as

testContReader5 :: Maybe [String]
testContReader5 = runReaderT (test7 ["sato","sakai"]) env1
testContReader6 :: Maybe [String]
testContReader6 = runReaderT (test7 ["sato","suzuki"]) env1


test8 :: [String] -> ReaderT Env Maybe [String]
test8 ns =  (runContT (callCCT $ \k -> test k ns ) ) return
  where
    -- k :: [String] -> CRM r b
    -- helper :: [String] -> ReaderT Env Maybe [String]
    -- helper ns = return ns
    test :: ([String] -> CRM r b) -> [String] -> CRM r [String]
    test  k ns = local (addEnvs [("sato","sapporo")] ) (test' k ns) 
    test' k [] = return []
    test' k (n:ns) = if n == ""
                     then
                       liftMaybeToCRM Nothing
                     else
                       do ma <- asks $ lookup n
                          a <-  liftMaybeToCRM ma
                          as <- test' k ns
                          return $ a:as
    liftMaybeToCRM :: Maybe a -> CRM r a
    liftMaybeToCRM = lift . lift 

      
testContReader7 = runReaderT (test8 ["sato","sakai"]) env1
testContReader8 = runReaderT (test8 ["sato",""]) env1

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


--- ContT + IO

-- callCCT :: ( (a -> ContT r IO b) -> ContT r IO a) -> ContT r IO a
-- k :: a -> ContT r IO b
-- func k :: ContT r IO a
-- callCCT(..) :: ContT r IO a
-- (runContT callCCT()) :: ( [String] -> IO [String] ) -> IO [String]


instance (MonadIO m) => MonadIO (ContT r m) where
  -- liftIO :: IO a -> ContT r IO a
  -- ContT r IO a :: (a -> IO r) -> IO r
  liftIO = lift . liftIO 

contIO  :: IO [String]
contIO = runContT (callCCT $ \k -> func k) return
  where
    func :: ([String] -> ContT [String] IO [String]) -> ContT [String] IO [String]
    func k = do
      liftIO $ putStr "input a> "
      a <- liftIO $ getLine
      liftIO $ putStr "input b> "
      b <- liftIO $ getLine
      if b == ""
        then
        do
          liftIO $ putStrLn $ "b empty. exit"
          k []
        else
        return []
      liftIO $ putStr "input c> "
      c <- liftIO $ getLine
      liftIO $ putStrLn $ "Chars are " ++ a ++ " " ++ b ++ " " ++ c
      return [a,b,c]

--- Application : for-loop 
  
forLoop :: Monad m => [a] -> (a -> ContT () m c) -> m ()
forLoop xs func = runContT (loop xs) return
  where
    loop []     = return ()
    loop (x:xs) = do func x
                     loop xs
      
breakOut :: Monad m => ContT () m c
breakOut = CT (\_ -> return () )

test10 = forLoop [0..] $ \i -> do
  if i > 10 then breakOut else return ()
  lift $ putStrLn $ "Number: " ++ show i

--- goto

-- callCCT :: ( (a -> ContT r IO b) -> ContT r IO a) -> ContT r IO a
-- out :: (a -> ContT r IO b) -> ContT r IO a
-- fn :: a -> ContT r IO b
-- fn :: ContT r IO a

goto :: ContT r IO (ContT r IO b)
goto = callCCT $ \out -> let fn = out fn
                         in return fn

test11 :: ContT r IO ()
test11 = do
  label1 <- goto
  lift $ putStrLn "1"
  label2 <- goto
  lift $ putStrLn "2"
  line <-lift $ getLine
  case line of
    "1" -> label1
    "2" -> label2
    _   -> return ()
  lift $ putStrLn "end"

  

  
  
