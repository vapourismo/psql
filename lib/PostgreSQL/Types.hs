{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module PostgreSQL.Types
  ( Value (..)
  , RegType (..)
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
  , ColumnNum (..)
  , RowNum (..)
  )
where

import           Control.Exception (Exception)
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty)
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Database.PostgreSQL.LibPQ as PQ
import           Foreign.C.Types (CInt)

-- | Value
data Value
  = Null
  | Value ByteString
  deriving (Show, Eq, Ord)

instance IsString Value where
  fromString = Value . fromString

-- | Postgre's regtype
newtype RegType = RegType
  { unRegType :: Text }
  deriving newtype (Show, Read, Eq, Ord, IsString)

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
    { processorError_column :: ColumnNum
    , processorError_type :: PQ.Oid
    , processorError_format :: PQ.Format
    , processorError_columnError :: ParserError
    }
  | CellParserError
    { processorError_column :: ColumnNum
    , processorError_type :: PQ.Oid
    , processorError_format :: PQ.Format
    , processorError_row :: RowNum
    , processorError_value :: Value
    , processorError_cellError :: Text
    }
  | NotEnoughColumns
    { processorError_wantedColumns :: ColumnNum
    , processorError_haveColumns :: ColumnNum
    }
  | MissingNamedColumn
    { processorError_wantedColumnName :: ByteString
    }
  deriving stock (Show, Eq, Ord)

type ProcessorErrors = NonEmpty ProcessorError

-- | Error that occurs when validating the result
data ResultError
  = BadResultStatus
    { resultError_status :: ByteString }
  | NoRows
  | MultipleRows
    { resultError_numRows :: RowNum }
  | FailedToParseAffectedRows
    { resultError_message :: Text }
  deriving stock (Show, Eq, Ord)

type ResultErrors = NonEmpty ResultError

data Error
  = ErrorDuringProcessing ProcessorError
  -- ^ Occurs when processing the result table
  | ErrorDuringValidation ResultError
  -- ^ Occurs when validating the result object
  deriving stock (Show, Eq, Ord)
  deriving anyclass Exception

type Errors = NonEmpty Error

-- | Numberic column identifier
newtype ColumnNum = ColumnNum
  { fromColumnNum :: PQ.Column }
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Num, Integral, Real) via CInt

-- | Numberic row identifier
newtype RowNum = RowNum
  { fromRowNum :: PQ.Row }
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Num, Integral, Real) via CInt
