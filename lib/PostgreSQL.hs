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
  , Query.queryWith

    -- ** Preparation
  , Query.withPreparedStatement

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
  , Statement.string
  , Statement.param
  , Statement.paramWith
  , Statement.constant
  , Statement.renderTemplate

    -- * Result processing
  , Result.Result
  , Result.ignored
  , Result.single
  , Result.first
  , Result.many
  , Result.affectedRows

    -- ** Rows
  , Row.AutoRow (..)
  , Row.genericRow

    -- ** Columns
  , Column.AutoColumn (..)
  , Column.Readable (..)

    -- * Errors
  , Types.Error (..)
  , Types.Errors
  , Types.ProcessorError (..)
  , Types.ProcessorErrors
  , Types.ResultError (..)
  , Types.ResultErrors
  , Types.ParserError (..)
  , Types.ParserErrors

    -- * Common types
  , Types.Format (..)
  , Types.Oid
  , Types.ColumnNum
  , Types.RowNum
  , Types.Value (..)
  , Column.RawValue (..)
  )
where

import qualified PostgreSQL.Class as Class
import qualified PostgreSQL.ConnectionPool as ConnectionPool
import qualified PostgreSQL.Query as Query
import qualified PostgreSQL.Result as Result
import qualified PostgreSQL.Result.Column as Column
import qualified PostgreSQL.Result.Row as Row
import qualified PostgreSQL.Statement as Statement
import qualified PostgreSQL.Types as Types
