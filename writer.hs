{-# LANGUAGE FlexibleInstances,FunctionalDependencies #-}


newtype Writer w a = W { runWriter :: (a,w) }

instance Functor (Writer w) where
  -- (a -> b) -> Writer w a -> Writer w b
  fmap f (W (a,w)) = W (f a,w)
  
instance (Monoid w) => Applicative (Writer w) where
  -- a -> Writer w a
  pure a = W (a,mempty)
  -- (Writer w f) -> (Writer w a) -> (Writer w b)
  W (f,w')  <*> W (a,w'') = W (f a, w' `mappend` w'')

instance (Monoid w) => Monad (Writer w) where
  W (a,w) >>= f = let (W (b,w')) = f a in W (b,w `mappend` w')


class (Monoid w, Monad m) => MonadWriter w m| m -> w where
  pass   :: m (a,w -> w) -> m a 
  listen :: m a -> m (a,w) 
  tell   :: w -> m () 


instance (Monoid w) => MonadWriter w (Writer w) where
  -- pass :: Writer w (a,w -> w) -> Writer w a
  pass (W ((a,wf),w)) = W (a,wf w)
  -- listen :: Writer w a -> Writer w (a,w)
  listen (W (a,w)) = W ((a,w),w)
  -- tell :: w -> m()
  tell w = W ((),w)

listens :: (MonadWriter w m) => (w -> w) -> m a -> m (a,w) 
listens f m = do (a,w) <- listen m; return (a,f w)
 
censor :: (MonadWriter w m) => (w -> w) -> m a -> m a 
censor f m = pass $ do a <- m; return (a,f)

writeWL :: String -> Writer [String] String 
writeWL str = do
  tell $ ["now i get string: " ++ str]
  return str

testWriter str = do
  abc <- writeWL "abc"
  def <- writeWL "def"
  ghi <- writeWL "ghi"
  tell ["and get"]
  return $ abc ++ def ++ ghi



  
