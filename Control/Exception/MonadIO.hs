
module Control.Exception.MonadIO
(
  MonadIO(..), CaughtMonadIO(..)
  , gtry, gtryJust, ioErrors
) where

import Control.Monad.Trans
import Control.Exception
import Control.Monad.Reader

gtry a = gcatch (liftM Right a) (return . Left)
gtryJust p a = gcatchJust p (liftM Right a) (return . Left)


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

 