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
import           PostgreSQL.Types (PackedParam (..), PackedParamPrepared (..), Value (..))

data Type
  = InferredType
  -- ^ Type is inferred on the server side
  | StaticType PQ.Oid
  -- ^ Explicit static type
  deriving (Show, Eq, Ord)

-- | Get the OID for the type.
typeOid :: Type -> PQ.Oid
typeOid = \case
  InferredType -> PQ.invalidOid
  StaticType oid -> oid

{-# INLINE typeOid #-}

-- | Static parameter information
data Info a = Info
  { info_type :: Type
  , info_typeName :: Maybe Text
  -- ^ This may be used as an explicit type annotation when used in a 'Statement' or 'Template'
  , info_format :: PQ.Format
  , info_pack :: a
  }
  deriving stock (Functor, Foldable, Traversable, Generic)

-- | Pack a parameter into a @postgresql-libpq@ format.
packParam :: Info Value -> PackedParam
packParam paramInfo = PackedParam $
  case info_pack paramInfo of
    Null -> Nothing
    Value datas -> Just (oid, datas, info_format paramInfo)
  where
    oid = typeOid $ info_type paramInfo

{-# INLINE packParam #-}

-- | Convert 'PackedParam'.
toPrepared :: PackedParam -> PackedParamPrepared
toPrepared (PackedParam param) = PackedParamPrepared $ do
  (_, datas, format) <- param
  pure (datas, format)

{-# INLINE toPrepared #-}

-- | Pack a parameter for a prepared query into a @postgresql-libpq@ format.
packParamPrepared :: Info Value -> PackedParamPrepared
packParamPrepared paramInfo = PackedParamPrepared $
  case info_pack paramInfo of
    Null -> Nothing
    Value datas -> Just (datas, info_format paramInfo)

{-# INLINE packParamPrepared #-}

-- | @a@ can be used as a parameter
class Param a where
  -- | Parameter information
  paramInfo :: Info (a -> Value)

instance Param Integer where
  paramInfo = Info
    { info_type = InferredType
    , info_typeName = Just "integer"
    , info_format = PQ.Text
    , info_pack = Value . ByteString.Char8.pack . show
    }

instance Param Double where
  paramInfo = Info
    { info_type = InferredType
    , info_typeName = Just "float8"
    , info_format = PQ.Text
    , info_pack = Value . ByteString.Char8.pack . show
    }

instance Param Text where
  paramInfo = Info
    { info_type = InferredType
    , info_typeName = Nothing
    , info_format = PQ.Text
    , info_pack = Value . encodeUtf8
    }

-- | Raw textual parameter
newtype RawText = RawText
  { unRawText :: ByteString }
  deriving (Show, Eq, Ord)

instance Param RawText where
  paramInfo = Info
    { info_type = InferredType
    , info_typeName = Nothing
    , info_format = PQ.Text
    , info_pack = Value . unRawText
    }

instance Param Value where
  paramInfo = Info
    { info_type = InferredType
    , info_typeName = Nothing
    , info_format = PQ.Text
    , info_pack = id
    }
