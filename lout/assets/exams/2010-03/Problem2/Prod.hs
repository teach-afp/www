module Problem2.Prod where
import Control.Monad (ap, liftM)

newtype Prod m n a = Prod {unProd :: (m a, n a)}

fstP :: Prod m n a -> m a
fstP = fst . unProd
sndP :: Prod m n a -> n a
sndP = snd . unProd

instance (Monad m, Monad n) => Monad (Prod m n) where
  return  =  returnProd
  (>>=)   =  bindProd

instance (MonadFail m, MonadFail n) => MonadFail (Prod m n) where
  fail    =  failProd

returnProd :: (Monad m, Monad n) => a -> Prod m n a
returnProd   x  =  Prod (return x, return x)

bindProd :: (Monad m, Monad n) =>
  Prod m n a -> (a -> Prod m n b) -> Prod m n b
bindProd mnx f  =  Prod ( fstP mnx >>= (fstP . f)
                        , sndP mnx >>= (sndP . f) )
failProd :: (MonadFail m, MonadFail n) => String -> Prod m n a
failProd s = Prod (fail s, fail s)


instance (Monad m, Monad n) => Applicative (Prod m n) where
  (<*>) = ap
  pure  = return

instance (Monad m, Monad n) => Functor (Prod m n) where
  fmap = liftM
