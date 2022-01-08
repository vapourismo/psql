{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module PostgreSQL.Param
  ( -- * Basics
    Type (..)
  , typeOid
  , Info (..)
  , Value (..)
  , PQ.Format (..)

  , PackedParam (..)
  , packParam
  , toPrepared
  , PackedParamPrepared (..)
  , packParamPrepared

    -- * Class
  , Param (..)
  , RawText (..)
  )
where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import qualified Database.PostgreSQL.LibPQ as PQ
import           GHC.Generics (Generic)
import           PostgreSQL.Types (PackedParam (..), PackedParamPrepared (..), RegType (..),
                                   Value (..))

-- | Parameter type
--
-- @since 0.0.0
data Type
  = InferredType
  -- ^ Type is inferred on the server side
  --
  -- @since 0.0.0
  | StaticType PQ.Oid
  -- ^ Explicit static type
  --
  -- @since 0.0.0
  deriving (Show, Eq, Ord)

-- | Get the OID for the type.
--
-- @since 0.0.0
typeOid :: Type -> PQ.Oid
typeOid = \case
  InferredType -> PQ.invalidOid
  StaticType oid -> oid

{-# INLINE typeOid #-}

-- | Static parameter information
--
-- @since 0.0.0
data Info a = Info
  { info_type :: Type
  , info_typeName :: Maybe Text
  -- ^ This may be used as an explicit type annotation when used in a 'Statement' or 'Template'
  , info_format :: PQ.Format
  , info_pack :: a
  }
  deriving stock (Functor, Foldable, Traversable, Generic)

-- | Pack a parameter into a @postgresql-libpq@ format.
--
-- @since 0.0.0
packParam :: Info Value -> PackedParam
packParam paramInfo = PackedParam $
  case info_pack paramInfo of
    Null -> Nothing
    Value datas -> Just (oid, datas, info_format paramInfo)
  where
    oid = typeOid $ info_type paramInfo

{-# INLINE packParam #-}

-- | Convert 'PackedParam'.
--
-- @since 0.0.0
toPrepared :: PackedParam -> PackedParamPrepared
toPrepared (PackedParam param) = PackedParamPrepared $ do
  (_, datas, format) <- param
  pure (datas, format)

{-# INLINE toPrepared #-}

-- | Pack a parameter for a prepared query into a @postgresql-libpq@ format.
--
-- @since 0.0.0
packParamPrepared :: Info Value -> PackedParamPrepared
packParamPrepared paramInfo = PackedParamPrepared $
  case info_pack paramInfo of
    Null -> Nothing
    Value datas -> Just (datas, info_format paramInfo)

{-# INLINE packParamPrepared #-}

-- | @a@ can be used as a parameter
--
-- @since 0.0.0
class Param a where
  -- | Parameter information
  --
  -- @since 0.0.0
  paramInfo :: Info (a -> Value)

-- | @since 0.0.0
instance Param Integer where
  paramInfo = Info
    { info_type = InferredType
    , info_typeName = Nothing
    , info_format = PQ.Text
    , info_pack = Value . ByteString.Char8.pack . show
    }

-- | @since 0.0.0
instance Param Double where
  paramInfo = Info
    { info_type = InferredType
    , info_typeName = Just "float8"
    , info_format = PQ.Text
    , info_pack = Value . ByteString.Char8.pack . show
    }

-- | @since 0.0.0
instance Param Text where
  paramInfo = Info
    { info_type = InferredType
    , info_typeName = Nothing
    , info_format = PQ.Text
    , info_pack = Value . encodeUtf8
    }

-- | @since 0.0.0
instance Param PQ.Oid where
  paramInfo = Info
    { info_type = InferredType
    , info_typeName = Just "oid"
    , info_format = PQ.Text
    , info_pack = \(PQ.Oid inner) -> Value $ ByteString.Char8.pack $ show inner
    }

-- | @since 0.0.0
instance Param RegType where
  paramInfo = Info
    { info_type = InferredType
    , info_typeName = Just "regtype"
    , info_format = PQ.Text
    , info_pack = Value . encodeUtf8 . unRegType
    }

-- | Raw textual parameter
--
-- @since 0.0.0
newtype RawText = RawText
  { unRawText :: ByteString }
  deriving (Show, Eq, Ord)

-- | @since 0.0.0
instance Param RawText where
  paramInfo = Info
    { info_type = InferredType
    , info_typeName = Nothing
    , info_format = PQ.Text
    , info_pack = Value . unRawText
    }

-- | @since 0.0.0
instance Param Value where
  paramInfo = Info
    { info_type = InferredType
    , info_typeName = Nothing
    , info_format = PQ.Text
    , info_pack = id
    }
