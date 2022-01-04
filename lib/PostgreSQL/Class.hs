{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module PostgreSQL.Class
  ( RunQuery (..)
  , runQueryThrow
  )
where

import qualified Control.Monad.Catch as Catch
import           Control.Monad.Trans (MonadTrans (lift))
import qualified Data.List.NonEmpty as NonEmpty
import           PostgreSQL.Query.Class (Query)
import           PostgreSQL.Types (Errors)

-- | PostgreSQL queries can be executed in @m@
class Query query => RunQuery query m | m -> query where
  -- | Run a query.
  runQuery :: query a -> m (Either Errors a)

-- | Like 'runQuery' but throws the first error instead.
runQueryThrow :: (Catch.MonadThrow m, RunQuery query m) => query a -> m a
runQueryThrow query = do
  err <- runQuery query
  either (Catch.throwM . NonEmpty.head) pure err

{-# INLINE runQueryThrow #-}

instance
  {-# OVERLAPPABLE #-}
  (RunQuery query m, Monad m, MonadTrans t)
  => RunQuery query (t m)
  where
    runQuery query = lift (runQuery query)

    {-# INLINE runQuery #-}
