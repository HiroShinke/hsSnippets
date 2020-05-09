


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

