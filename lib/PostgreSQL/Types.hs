{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module PostgreSQL.Types
  ( Value (..)
  , PackedParam (..)
  , PackedParamPrepared (..)
  , PQ.Format (..)
  , ParserError (..)
  , ParserErrors
  , ProcessorError (..)
  , ProcessorErrors
  , ResultError (..)
  , ResultErrors
  , Error (..)
  , Errors
  , Assembler
  )
where

import           Control.Exception (Exception)
import           Control.Monad.Error.Class (MonadError)
import           Data.ByteString (ByteString)
import           Data.Functor.Alt (Alt)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import qualified Database.PostgreSQL.LibPQ as PQ

-- | Value
data Value
  = Null
  | Value ByteString
  deriving (Show, Eq, Ord)

-- | Packed parameter
newtype PackedParam = PackedParam (Maybe (PQ.Oid, ByteString, PQ.Format))
  deriving newtype Show

-- | Packed parameter for a prepared query
newtype PackedParamPrepared = PackedParamPrepared (Maybe (ByteString, PQ.Format))
  deriving newtype Show

-- | Error that occurs when parsing a column
data ParserError
  = UnsupportedFormat PQ.Format
  | UnsupportedOid PQ.Oid
  deriving stock (Show, Eq, Ord)

type ParserErrors = NonEmpty ParserError

-- | Error that may occur during processing
data ProcessorError
  = ColumnParserError
    { processorError_column :: PQ.Column
    , processorError_type :: PQ.Oid
    , processorError_format :: PQ.Format
    , processorError_columnError :: ParserError
    }
  | CellParserError
    { processorError_column :: PQ.Column
    , processorError_type :: PQ.Oid
    , processorError_format :: PQ.Format
    , processorError_row :: PQ.Row
    , processorError_value :: Value
    , processorError_cellError :: Text
    }
  | NotEnoughColumns
    { processorError_wantedColumns :: PQ.Column
    , processorError_haveColumns :: PQ.Column
    }
  | MissingNamedColumn
    { processorError_wantedColumnName :: ByteString
    }
  deriving stock (Show, Eq, Ord)

type ProcessorErrors = NonEmpty ProcessorError

-- | Error that occurs when validating the result
newtype ResultError = ResultError
  { unResultError :: ByteString }
  deriving (Show, Eq, Ord)

type ResultErrors = NonEmpty ResultError

data Error
  = ErrorDuringProcessing ProcessorError
  -- ^ Occurs when processing the result table
  | ErrorDuringValidation ResultError
  -- ^ Occurs when validating the result object
  deriving stock (Show, Eq, Ord)
  deriving anyclass Exception

type Errors = NonEmpty Error

-- | Given a number of rows and a way to fetch each row, assemble the result.
type Assembler row result =
  forall n. (MonadError ProcessorErrors n, Alt n) => PQ.Row -> (PQ.Row -> n row) -> n result
