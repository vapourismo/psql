{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Things in this module are used for processing Postgres query result rows.
module PostgreSQL.Result.Row
  ( Row
  , runRow
  , ColumnRequest (..)
  , ColumnPosition (..)

    -- * Combinators
  , column
  , columnWith
  , fixedColumn
  , fixedColumnWith
  , namedColumn
  , namedColumnWith

    -- * Class
  , AutoRow (..)
  , genericRow
  , AutoColumnDelegate

    -- * Helpers
  , Fixed (..)
  , Named (..)
  )
where

import           Control.Applicative (liftA2)
import qualified Control.Monad.State.Strict as State
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import           Data.Data (Proxy (..))
import           Data.Functor.Apply (Apply (..))
import           Data.Functor.Identity (Identity (..))
import           Data.Void (Void)
import qualified GHC.Generics as Generics
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import           GHC.TypeNats (KnownNat, Nat, natVal)
import qualified PostgreSQL.Result.Column as Column
import           PostgreSQL.Types (ColumnNum)

-- | Position of a column
--
-- @since 0.0.0
data ColumnPosition
  = FixedColumn ColumnNum
  -- ^ Column is at a fixed index.
  --
  -- @since 0.0.0
  | NamedColumn ByteString
  -- ^ Column has a fixed name.
  --
  -- @since 0.0.0
  deriving stock (Show, Read, Eq, Ord)

-- | Request a column
--
-- @since 0.0.0
data ColumnRequest a = ColumnReqest
  { columnRequest_position :: ColumnPosition
  -- ^ Location of the column
  , columnRequest_parser :: Column.Column a
  -- ^ Parser for the column
  }
  deriving stock Functor

-- | Result row parser
--
-- @since 0.0.0
newtype Row a = Row
  { _unRow
      :: forall m row
      .  (Monad m, Applicative row)
      => (forall x. ColumnRequest x -> m (row x))
      -> State.StateT ColumnNum m (row a)
  }

-- | @since 0.0.0
instance Functor Row where
  fmap f (Row run) = Row (\liftRequest -> fmap f <$> run liftRequest)

  {-# INLINE fmap #-}

-- | @since 0.0.0
instance Applicative Row where
  pure x = Row $ \_liftRequest -> pure $ pure x

  {-# INLINE pure #-}

  Row f <*> Row x = Row $ \liftRequest -> liftA2 (<*>) (f liftRequest) (x liftRequest)

  {-# INLINE (<*>) #-}

-- | @since 0.0.0
instance Apply Row where
  (<.>) = (<*>)

-- | Translate a 'Row' expression.
--
-- @since 0.0.0
runRow
  :: (Monad m, Applicative row)
  => Row a
  -> (forall x. ColumnRequest x -> m (row x))
  -> m (row a)
runRow (Row run) liftRequest =
  State.evalStateT (run liftRequest) 0

-- | Floating column using the default 'Column.Column' for @a@
--
-- The position of this column is depenend on other floating columns left of it.
--
-- For example:
--
-- > foo = baz <$> column <*> column <*> column
-- > --            ^ A        ^ B        ^ C
--
-- Here, @A@ would be at index 0, @B@ at 1 and @C@ at 2.
-- Other non-floating columns do not impact the column indices.
--
-- @since 0.0.0
column :: Column.AutoColumn a => Row a
column = columnWith Column.autoColumn

{-# INLINE column #-}

-- | Same as 'column' but lets you specify the 'Column.Column'.
--
-- @since 0.0.0
columnWith :: Column.Column a -> Row a
columnWith column = Row $ \liftRequest -> do
  col <- State.state (\col -> (col, col + 1))
  State.lift $ liftRequest ColumnReqest
    { columnRequest_position = FixedColumn col
    , columnRequest_parser = column
    }

{-# INLINE columnWith #-}

-- | Fixed-position column using the default 'Column.Column' for @a@
--
-- @since 0.0.0
fixedColumn :: Column.AutoColumn a => ColumnNum -> Row a
fixedColumn num = fixedColumnWith num Column.autoColumn

{-# INLINE fixedColumn #-}

-- | Same as 'fixedColumn' but lets you specify the 'Column.Column'.
--
-- @since 0.0.0
fixedColumnWith :: ColumnNum -> Column.Column a -> Row a
fixedColumnWith number column = Row $ \liftRequest -> State.lift $
  liftRequest ColumnReqest
    { columnRequest_position = FixedColumn number
    , columnRequest_parser = column
    }

{-# INLINE fixedColumnWith #-}

-- | Named column using the default 'Column.Column' for @a@
--
-- @since 0.0.0
namedColumn :: Column.AutoColumn a => ByteString -> Row a
namedColumn name = namedColumnWith name Column.autoColumn

{-# INLINE namedColumn #-}

-- | Same as 'namedColumn' but lets you specify the 'Column.Column'.
--
-- @since 0.0.0
namedColumnWith :: ByteString -> Column.Column a -> Row a
namedColumnWith name column = Row $ \liftRequest -> State.lift $
  liftRequest ColumnReqest
    { columnRequest_position = NamedColumn name
    , columnRequest_parser = column
    }

{-# INLINE namedColumnWith #-}

-- | Generic row parser
--
-- You can use this with your 'Generics.Generic'-implementing data types.
--
-- > data Foo = Foo
-- >   { bar :: Integer
-- >   , baz :: Text
-- >   }
-- >   deriving Generic
-- >
-- > fooRow :: Row Foo
-- > fooRow = genericRow
--
-- @since 0.0.0
genericRow :: (Generics.Generic a, AutoRow (Generics.Rep a Void)) => Row a
genericRow = Generics.to @_ @Void <$> autoRow

{-# INLINE genericRow #-}

-- | Value for a column at a fixed location
--
-- @since 0.0.0
newtype Fixed (index :: Nat) a = Fixed
  { fromFixed :: a }

-- | Value for a named column
--
-- @since 0.0.0
newtype Named (name :: Symbol) a = Named
  { fromNamed :: a }

-- | This class is used to intercept instance heads like 'Fixed' and 'Named' that have special
-- additional meaning. For most cases it will delegate to 'Column.AutoColumn'.
--
-- Use this class instead of 'Column.AutoColumn' when implementing 'AutoRow' instances.
--
-- @since 0.0.0
class AutoColumnDelegate a where
  autoColumnDelegate :: Row a

-- | Uses 'fixedColumn' with @index@ to construct the 'Row'
--
-- @since 0.0.0
instance (KnownNat index, Column.AutoColumn a) => AutoColumnDelegate (Fixed index a) where
  autoColumnDelegate = Fixed <$> fixedColumn (fromIntegral (natVal @index Proxy))

-- | Uses 'namedColumn' with @name@ to construct the 'Row'
--
-- @since 0.0.0
instance (KnownSymbol name, Column.AutoColumn a) => AutoColumnDelegate (Named name a) where
  autoColumnDelegate = Named <$> namedColumn (Char8.pack (symbolVal @name Proxy))

-- | Passthrough to 'Column.AutoColumn'
--
-- @since 0.0.0
instance {-# OVERLAPPABLE #-} Column.AutoColumn a => AutoColumnDelegate a where
  autoColumnDelegate = column

-- | Default row parser for a type
--
-- @since 0.0.0
class AutoRow a where
  -- | Default row parser for @a@
  --
  -- You may omit a definition for 'autoRow' if @a@ implements 'Generics.Generic'.
  --
  -- @since 0.0.0
  autoRow :: Row a

  default autoRow :: (Generics.Generic a, AutoRow (Generics.Rep a Void)) => Row a
  autoRow = genericRow

  {-# INLINE autoRow #-}

-- | @since 0.0.0
instance AutoColumnDelegate a => AutoRow (Generics.K1 tag a x) where
  autoRow = Generics.K1 <$> autoColumnDelegate

  {-# INLINE autoRow #-}

-- | @since 0.0.0
instance AutoRow (f x) => AutoRow (Generics.M1 tag meta f x) where
  autoRow = Generics.M1 <$> autoRow

  {-# INLINE autoRow #-}

-- | @since 0.0.0
instance (AutoRow (lhs x), AutoRow (rhs x)) => AutoRow ((Generics.:*:) lhs rhs x) where
  autoRow = (Generics.:*:) <$> autoRow <*> autoRow

  {-# INLINE autoRow #-}

-- | @since 0.0.0
instance AutoColumnDelegate a => AutoRow (Identity a)

-- | @since 0.0.0
instance
  ( AutoColumnDelegate a
  , AutoColumnDelegate b
  )
  => AutoRow (a, b)

-- | @since 0.0.0
instance
  ( AutoColumnDelegate a
  , AutoColumnDelegate b
  , AutoColumnDelegate c
  )
  => AutoRow (a, b, c)

-- | @since 0.0.0
instance
  ( AutoColumnDelegate a
  , AutoColumnDelegate b
  , AutoColumnDelegate c
  , AutoColumnDelegate d
  )
  => AutoRow (a, b, c, d)

-- | @since 0.0.0
instance
  ( AutoColumnDelegate a
  , AutoColumnDelegate b
  , AutoColumnDelegate c
  , AutoColumnDelegate d
  , AutoColumnDelegate e
  )
  => AutoRow (a, b, c, d, e)

-- | @since 0.0.0
instance
  ( AutoColumnDelegate a
  , AutoColumnDelegate b
  , AutoColumnDelegate c
  , AutoColumnDelegate d
  , AutoColumnDelegate e
  , AutoColumnDelegate f
  )
  => AutoRow (a, b, c, d, e, f)

-- | @since 0.0.0
instance
  ( AutoColumnDelegate a
  , AutoColumnDelegate b
  , AutoColumnDelegate c
  , AutoColumnDelegate d
  , AutoColumnDelegate e
  , AutoColumnDelegate f
  , AutoColumnDelegate g
  )
  => AutoRow (a, b, c, d, e, f, g)
