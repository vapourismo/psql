{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module PostgreSQL.ConnectionPool
  ( ConnectionPoolT (..)
  , runConnectionPoolT
  , defaultConnectionPoolSettings
  , connectionPoolMetrics
  )
where

import qualified Control.Monad.Catch as Catch
import           Control.Monad.Conc.Class (MonadConc)
import qualified Control.Monad.Except as Except
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Reader as Reader
import           Control.Monad.State.Class (MonadState)
import           Control.Monad.Trans (MonadTrans, lift)
import           Control.Monad.Writer.Class (MonadWriter)
import qualified Database.PostgreSQL.LibPQ as PQ
import           Numeric.Natural (Natural)
import qualified PostgreSQL.Class as Class
import qualified PostgreSQL.Query as Query
import qualified Simpoole as Pool
import qualified Simpoole.Monad as Pool.Monad
import qualified Simpoole.Monad.Internal as Pool.Monad

-- | Interpreter for 'RunPostgreSQL' which dispatches queries to a pool of database connections
--
-- @since 0.0.0
newtype ConnectionPoolT m a = ConnectionPoolT
  { unConnectionPoolT :: Pool.Monad.PoolT PQ.Connection m a }
  deriving newtype
    ( Functor -- ^ @since 0.0.0
    , Applicative -- ^ @since 0.0.0
    , Monad -- ^ @since 0.0.0
    , MonadFail -- ^ @since 0.0.0
    , MonadIO -- ^ @since 0.0.0
    , MonadState s -- ^ @since 0.0.0
    , Except.MonadError e -- ^ @since 0.0.0
    , MonadWriter w -- ^ @since 0.0.0
    , Catch.MonadThrow -- ^ @since 0.0.0
    , Catch.MonadCatch -- ^ @since 0.0.0
    , Catch.MonadMask -- ^ @since 0.0.0
    , MonadConc -- ^ @since 0.0.0
    )

-- | @since 0.0.0
instance MonadTrans ConnectionPoolT where
  lift = ConnectionPoolT . Reader.lift

-- | @since 0.0.0
instance
  (Catch.MonadMask m, MonadIO m)
  => Class.RunQuery (Query.QueryT m) (ConnectionPoolT m)
  where
    runQuery query = ConnectionPoolT $ Pool.Monad.withResource $ \conn ->
      lift $ Query.runQueryT conn query

    {-# INLINE runQuery #-}

-- | Default settings for the connection pool
--
-- @since 0.0.0
defaultConnectionPoolSettings :: Pool.Settings
defaultConnectionPoolSettings = Pool.defaultSettings
  { Pool.settings_idleTimeout = Just 60 -- seconds
  , Pool.settings_returnPolicy = Pool.ReturnToFront
  , Pool.settings_maxLiveLimit = Just 5
  }

-- | Run connection pool transformer.
--
-- @since 0.0.0
runConnectionPoolT
  :: (MonadIO m, MonadConc m)
  => m PQ.Connection
  -- ^ Action to establish a new connection
  -> Pool.Settings
  -- ^ Connection pool settings
  -> ConnectionPoolT m a
  -- ^ Transformer to run
  -> m a
runConnectionPoolT connect poolSettings (ConnectionPoolT inner) = do
  pool <- Pool.newPool connect (liftIO . PQ.finish) poolSettings
  Pool.Monad.runPoolT pool inner

{-# INLINE runConnectionPoolT #-}

-- | Retrieve the connection pool metrics.
--
-- @since 0.0.0
connectionPoolMetrics :: ConnectionPoolT m (Pool.Metrics Natural)
connectionPoolMetrics = ConnectionPoolT Pool.Monad.metricsPoolT
