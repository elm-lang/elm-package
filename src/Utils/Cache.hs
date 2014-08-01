module Utils.Cache where

cacheComputation :: Monad m => m a -> m (Maybe a) -> (a -> m ()) -> m a
cacheComputation uncached cached store = do
  fromCache <- cached
  case fromCache of
    Just x -> return x
    Nothing -> do
      result <- uncached
      store result
      return result
