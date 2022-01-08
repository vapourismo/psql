{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module PostgreSQL.Result.Class
  ( HasResult (..)
  )
where

import           Control.Monad.Trans (MonadTrans, lift)
import           Data.ByteString (ByteString)
import qualified Database.PostgreSQL.LibPQ as PQ
import           PostgreSQL.Types (ColumnNum, RowNum, Value)

-- | @m@ provices access to a query result
class HasResult m where
  -- | Number of columns
  numColumns :: m ColumnNum

  -- | Number of rows
  numRows :: m RowNum

  -- | Type of a column
  columnType :: ColumnNum -> m PQ.Oid

  -- | Format of cells belonging to a column
  columnFormat :: ColumnNum -> m PQ.Format

  -- | Value of a cell
  cellValue :: ColumnNum -> RowNum -> m Value

  -- | Column number of a named column
  columnFromName :: ByteString -> m (Maybe ColumnNum)

instance {-# OVERLAPPABLE #-} (MonadTrans t, HasResult m, Monad m) => HasResult (t m) where
  numColumns = lift numColumns

  numRows = lift numRows

  columnType col = lift $ columnType col

  columnFormat col = lift $ columnFormat col

  cellValue col row = lift $ cellValue col row

  columnFromName col = lift $ columnFromName col
