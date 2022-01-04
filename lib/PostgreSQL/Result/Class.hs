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
import           PostgreSQL.Types (Value)

-- | @m@ provices access to a query result
class HasResult m where
  -- | Number of columns
  numColumns :: m PQ.Column

  -- | Number of rows
  numRows :: m PQ.Row

  -- | Type of a column
  columnType :: PQ.Column -> m PQ.Oid

  -- | Format of cells belonging to a column
  columnFormat :: PQ.Column -> m PQ.Format

  -- | Value of a cell
  cellValue :: PQ.Column -> PQ.Row -> m Value

  -- | Column number of a named column
  columnFromName :: ByteString -> m (Maybe PQ.Column)

instance {-# OVERLAPPABLE #-} (MonadTrans t, HasResult m, Monad m) => HasResult (t m) where
  numColumns = lift numColumns

  numRows = lift numRows

  columnType col = lift $ columnType col

  columnFormat col = lift $ columnFormat col

  cellValue col row = lift $ cellValue col row

  columnFromName col = lift $ columnFromName col
