

import Data.Char

class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

class (Monad m) => MonadIO m where
  liftIO :: IO a -> m a

newtype STT s m a = T (s -> m (a,s))

runSTT (T stt) = stt
appT (T stt) x = stt x

instance (Monad m) => Functor (STT s m) where
  fmap f (T st) = T ( \s -> do (a,s') <- st s
                               return (f a,s')
                    )

instance (Monad m) => Applicative (STT s m) where
  pure a = T (\s -> return (a,s) )
  (T ftt) <*> stt = T (\s -> do (f,s') <- ftt s
                                appT (fmap f stt) s'
                          )

instance (Monad m) => Monad (STT s m) where
  (T stt) >>= f = T( \s -> do (a,s') <- stt s
                              let (T stt2) = (f a)
                              stt2 s'
                   )

instance MonadTrans (STT s) where
  -- lift :: (Monad m) => m a -> t m a
  lift ma = T (\s -> do a <- ma
                        return (a,s) )

get'    :: Monad m => STT s m s
get'    = T (\s -> return (s,s))
put' :: Monad m => s -> STT s m ()
put' s' = T (\s -> return ((),s') )


----- StateT + Maybe
          
getNum ::  String -> Int -> Maybe Int
getNum str n=
  let c = str !! n
  in if isDigit c then Just (digitToInt c) else Nothing
  

getNumT :: String -> STT Int Maybe Int
getNumT str = T ( \n -> do i <- getNum str n  
                           return (i,n+1)
                )
  
sampleSTT str = do a <- getNumT str
                   b <- getNumT str
                   c <- getNumT str
                   return $ a * b * c

getNumT' :: String -> STT Int Maybe Int
getNumT' str = do n <- get'
                  i <- lift $ getNum str n
                  n <- put' $ (n + 1)
                  return i

sampleSTT' str = do a <- getNumT' str
                    b <- getNumT' str
                    c <- getNumT' str
                    return $ a * b * c

                   
testSTT1 = appT (sampleSTT "1234567") 1
testSTT2 = appT (sampleSTT "12A4567") 1

testSTT1' = appT (sampleSTT' "1234567") 1
testSTT2' = appT (sampleSTT' "12A4567") 1

----- StateT + []


getSub str n = map (helper n) [n..(length(str)-1)]
  where helper i j = map (str!!) [i..j]

getSubT :: String -> STT Int [] String
getSubT str = T ( \n -> do s <- getSub str n  
                           return (s,n+length(s))
                )

getSubT' :: String -> STT Int [] String
getSubT' str = do n <-get'
                  s <- lift $ getSub str n
                  put' $ n + length(s)
                  return s

sampleSTT2 str = do a <- getSubT str
                    b <- getSubT str
                    c <- getSubT str
                    return $ a ++ "*" ++ b ++ "*" ++ c

sampleSTT2' str = do a <- getSubT' str
                     b <- getSubT' str
                     c <- getSubT' str
                     return $ a ++ "*" ++ b ++ "*" ++ c
                    
