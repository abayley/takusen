
module Control.Exception.MonadIO
(
  CaughtMonadIO(..)
  , gtry, gtryJust, gbracket, gfinally
) where

import Control.Monad.Trans
import Control.Exception
import Control.Monad.Reader

gtry :: (CaughtMonadIO m) => m b -> m (Either Exception b)
gtry a = gcatch (liftM Right a) (return . Left)

gtryJust :: (CaughtMonadIO m) =>
  (Exception -> Maybe b) -> m b1 -> m (Either b b1)
gtryJust p a = gcatchJust p (liftM Right a) (return . Left)

gbracket :: (CaughtMonadIO m) =>
  m t -> (t -> m a) -> (t -> m b) -> m b
gbracket acq rel act = do
  r <- acq
  gcatch (do
      v <- act r
      rel r
      return v
    )
    (\e -> rel r >> throw e)

gfinally :: (CaughtMonadIO m) => m t -> m a -> m t
gfinally a f = gbracket (return ()) (const f) (const a)

class MonadIO m => CaughtMonadIO m where
  gcatch :: m a -> (Exception -> m a) -> m a
  gcatchJust :: (Exception -> Maybe b) -> m a -> (b -> m a) -> m a

instance CaughtMonadIO IO where
  gcatch = Control.Exception.catch
  gcatchJust = catchJust

instance CaughtMonadIO m => CaughtMonadIO (ReaderT a m) where
  gcatch a h = ReaderT $ 
    \r -> gcatch (runReaderT a r) (\e -> runReaderT (h e) r)
  gcatchJust p a h = ReaderT $
    \r -> gcatch (runReaderT a r) (\e ->
       case p e of
         Nothing -> throw e
         Just e' -> runReaderT (h e') r
       )

