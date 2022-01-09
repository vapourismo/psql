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

  , Types.Value (..)
  , Types.Oid
  , Types.Format (..)

  , Types.PackedParam (..)
  , packParam
  , toPrepared
  , Types.PackedParamPrepared (..)
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
import           Database.PostgreSQL.LibPQ (invalidOid)
import           GHC.Generics (Generic)
import qualified PostgreSQL.Types as Types

-- | Parameter type
--
-- @since 0.0.0
data Type
  = InferredType
  -- ^ Type is inferred on the server side
  --
  -- @since 0.0.0
  | StaticType Types.Oid
  -- ^ Explicit static type
  --
  -- @since 0.0.0
  deriving (Show, Eq, Ord)

-- | Get the OID for the type.
--
-- @since 0.0.0
typeOid :: Type -> Types.Oid
typeOid = \case
  InferredType -> invalidOid
  StaticType oid -> oid

{-# INLINE typeOid #-}

-- | Static parameter information
--
-- @since 0.0.0
data Info a = Info
  { info_type :: Type
  , info_typeName :: Maybe Text
  -- ^ This may be used as an explicit type annotation when used in a 'Statement' or 'Template'
  , info_format :: Types.Format
  , info_pack :: a
  }
  deriving stock (Functor, Foldable, Traversable, Generic)

-- | Pack a parameter into a @postgresql-libpq@ format.
--
-- @since 0.0.0
packParam :: Info Types.Value -> Types.PackedParam
packParam paramInfo = Types.PackedParam $
  case info_pack paramInfo of
    Types.Null -> Nothing
    Types.Value datas -> Just (oid, datas, info_format paramInfo)
  where
    oid = typeOid $ info_type paramInfo

{-# INLINE packParam #-}

-- | Convert 'PackedParam'.
--
-- @since 0.0.0
toPrepared :: Types.PackedParam -> Types.PackedParamPrepared
toPrepared (Types.PackedParam param) = Types.PackedParamPrepared $ do
  (_, datas, format) <- param
  pure (datas, format)

{-# INLINE toPrepared #-}

-- | Pack a parameter for a prepared query into a @postgresql-libpq@ format.
--
-- @since 0.0.0
packParamPrepared :: Info Types.Value -> Types.PackedParamPrepared
packParamPrepared paramInfo = Types.PackedParamPrepared $
  case info_pack paramInfo of
    Types.Null -> Nothing
    Types.Value datas -> Just (datas, info_format paramInfo)

{-# INLINE packParamPrepared #-}

-- | @a@ can be used as a parameter
--
-- @since 0.0.0
class Param a where
  -- | Parameter information
  --
  -- @since 0.0.0
  paramInfo :: Info (a -> Types.Value)

-- | @since 0.0.0
instance Param Integer where
  paramInfo = Info
    { info_type = InferredType
    , info_typeName = Nothing
    , info_format = Types.Text
    , info_pack = Types.Value . ByteString.Char8.pack . show
    }

-- | @since 0.0.0
instance Param Double where
  paramInfo = Info
    { info_type = InferredType
    , info_typeName = Just "float8"
    , info_format = Types.Text
    , info_pack = Types.Value . ByteString.Char8.pack . show
    }

-- | @since 0.0.0
instance Param Text where
  paramInfo = Info
    { info_type = InferredType
    , info_typeName = Nothing
    , info_format = Types.Text
    , info_pack = Types.Value . encodeUtf8
    }

-- | @since 0.0.0
instance Param Types.Oid where
  paramInfo = Info
    { info_type = InferredType
    , info_typeName = Just "oid"
    , info_format = Types.Text
    , info_pack = \(Types.Oid inner) -> Types.Value $ ByteString.Char8.pack $ show inner
    }

-- | @since 0.0.0
instance Param Types.RegType where
  paramInfo = Info
    { info_type = InferredType
    , info_typeName = Just "regtype"
    , info_format = Types.Text
    , info_pack = Types.Value . encodeUtf8 . Types.unRegType
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
    , info_format = Types.Text
    , info_pack = Types.Value . unRawText
    }

-- | @since 0.0.0
instance Param Types.Value where
  paramInfo = Info
    { info_type = InferredType
    , info_typeName = Nothing
    , info_format = Types.Text
    , info_pack = id
    }
