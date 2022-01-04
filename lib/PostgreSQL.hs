{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module PostgreSQL
  ( -- * Statements
    Statement.Statement
  , Statement.stmt

  , Statement.PreparedStatement

    -- * Queries
    -- ** Execution
  , Query.execute
  , Query.execute_
  , Query.query
  , Query.queryCustom
  , Query.queryWith
  , Query.queryCustomWith

    -- ** Preparation
  , Query.withPreparedStatement

    -- ** Results
  , Query.QueryResult (..)
  , Query.genericQueryProcessor

    -- ** Evaluation
  , Query.Query
  , Class.RunQuery (..)
  , Class.runQueryThrow

    -- ** Interpreters
  , ConnectionPoolT
  , runConnectionPoolT
  , defaultConnectionPoolSettings

    -- * Templates
  , Statement.Template
  , Statement.tpl
  , Statement.code
  , Statement.identifier
  , Statement.param
  , Statement.paramWith
  , Statement.constant
  , Statement.renderTemplate

    -- * Result processing
  , Result.Processor
  , Result.column
  , Result.columnWith
  , Result.namedColumn
  , Result.namedColumnWith

  , Column.ColumnResult (..)

    -- * Errors
  , Error (..)
  , Errors
  , Result.ProcessorError (..)
  , Result.ProcessorErrors
  , Result.ResultError (..)
  , Result.ResultErrors
  , Column.ParserError (..)
  , Column.ParserErrors

    -- * Common types
  , PQ.Format (..)
  , PQ.Oid
  , PQ.Column
  , PQ.Row
  , Column.RawValue (..)
  , Assembler
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
import qualified PostgreSQL.Class as Class
import qualified PostgreSQL.Column as Column
import qualified PostgreSQL.Query as Query
import qualified PostgreSQL.Result as Result
import qualified PostgreSQL.Statement as Statement
import           PostgreSQL.Types (Assembler, Error (..), Errors)
import qualified Simpoole as Pool
import qualified Simpoole.Monad as Pool.Monad
import qualified Simpoole.Monad.Internal as Pool.Monad

---

-- | Interpreter for 'RunPostgreSQL' which dispatches queries to a pool of database connections
newtype ConnectionPoolT m a = ConnectionPoolT
  { _unConnectionPoolT :: Pool.Monad.PoolT PQ.Connection m a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFail
    , MonadIO
    , MonadState s
    , Except.MonadError e
    , MonadWriter w
    , Catch.MonadThrow
    , Catch.MonadCatch
    , Catch.MonadMask
    , MonadConc
    )

instance MonadTrans ConnectionPoolT where
  lift = ConnectionPoolT . Reader.lift

instance
  (Catch.MonadMask m, MonadIO m)
  => Class.RunQuery (Query.QueryT m) (ConnectionPoolT m)
  where
    runQuery query = ConnectionPoolT $ Pool.Monad.withResource $ \conn ->
      lift $ Query.runQueryT conn query

    {-# INLINE runQuery #-}

-- | Default settings for the connection pool
defaultConnectionPoolSettings :: Pool.Settings
defaultConnectionPoolSettings = Pool.defaultSettings
  { Pool.settings_idleTimeout = Just 60 -- seconds
  , Pool.settings_returnPolicy = Pool.ReturnToFront
  , Pool.settings_maxLiveLimit = Just 5
  }

-- | Run connection pool transformer.
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
