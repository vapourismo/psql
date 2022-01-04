{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  , ConnectionPool.ConnectionPoolT
  , ConnectionPool.runConnectionPoolT
  , ConnectionPool.defaultConnectionPoolSettings

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

import qualified Database.PostgreSQL.LibPQ as PQ
import qualified PostgreSQL.Class as Class
import qualified PostgreSQL.Column as Column
import qualified PostgreSQL.ConnectionPool as ConnectionPool
import qualified PostgreSQL.Query as Query
import qualified PostgreSQL.Result as Result
import qualified PostgreSQL.Statement as Statement
import           PostgreSQL.Types (Assembler, Error (..), Errors)
