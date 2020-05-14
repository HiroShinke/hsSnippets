
{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances #-}

newtype Reader e a = R { runReader :: (e -> a) }

instance Functor (Reader e) where
  -- fmap :: ( a -> b ) -> Reader e a -> Reader e b
  fmap f (R ma)  = R (\e -> f (ma e) )

instance Applicative (Reader e) where
  -- pure :: a -> Reader e a
  pure a = R (\_ -> a)
  -- (<*>) :: Reader e (a -> b)  -> Reader e a -> Reader e b
  (R mf) <*> ma = R (\e -> (runReader (fmap (mf e) ma)) e )

instance Monad (Reader e) where
  -- (>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
  (R ma) >>= f = R (\e -> runReader (f (ma e) ) e )
  
class MonadReader e m | m -> e where 
    ask   :: m e
    local :: (e -> e) -> m a -> m a 
 
instance MonadReader e (Reader e) where
    -- ask :: Reader e e 
    ask       = R id
    -- local :: (e -> e) -> Reader e -> Reader e
    local f c = R $ \e -> runReader c (f e) 
 
asks :: (Monad m, MonadReader e m) => (e -> a) -> m a 
asks sel = ask >>= return . sel


type Env = [(String,String)]  
  
lookupVar :: String -> Env -> Maybe String
lookupVar name env = lookup name env

env1 = [("shinke","yokohama"),
        ("tanaka","tokyo"),
        ("sakai", "osaka")]

addEnvs :: [(String,String)] -> Env -> Env
addEnvs kv env = kv ++ env
       

reader :: String -> Reader Env String
reader s =R ( \e -> readerFunc s e )

readerFunc :: String -> Env -> String
readerFunc s e = case lookupVar s e of
                   Just p -> p
                   Nothing -> "nowhere"

testReader :: Reader Env String
testReader = do
  ad1 <- reader "shinke"
  ad2 <- reader "tanaka"
  ad3 <- reader "sakai"
  return $ ad1 ++ "*" ++ ad2 ++ "*" ++ ad3

testReader2 :: Reader Env String
testReader2 = local ( addEnvs [("shinke","kawasaki"),
                               ("okazaki","kobe")] )
  (
  do ad1 <- reader "shinke"
     ad2 <- reader "tanaka"
     ad3 <- reader "sakai"
     ad4 <- reader "okazaki"
     return $ ad1 ++ "*" ++ ad2 ++ "*" ++ ad3 ++ "*" ++ ad4
  )

testReader3 :: Reader Env String
testReader3 = do
  ad1 <- asks $ (readerFunc "shinke")
  ad2 <- asks $ (readerFunc "tanaka")
  ad3 <- asks $ (readerFunc "sakai")
  return $ ad1 ++ "*" ++ ad2 ++ "*" ++ ad3
  



  
