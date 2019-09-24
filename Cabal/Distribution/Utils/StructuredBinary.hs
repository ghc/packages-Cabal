{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- |
--
-- Copyright: (c) 2019 Oleg Grenrus
--
-- Structurally tag binary serialisaton stream.
-- Useful when most 'Binary' instances are 'Generic' derived.
--
-- Say you have a data type
--
-- @
-- data Record = Record
--   { _recordFields  :: HM.HashMap Text (Integer, ByteString)
--   , _recordEnabled :: Bool
--   }
--   deriving (Eq, Show, Generic)
--
-- instance 'Binary' Record
-- instance 'Structured' Record
-- @
--
-- then you can serialise and deserialise @Record@ values with a structure tag by simply
--
-- @
-- 'structuredEncode' record :: 'LBS.ByteString'
-- 'structuredDecode' lbs :: IO Record
-- @
--
-- If structure of @Record@ changes in between, deserialisation will fail early.
--
module Distribution.Utils.StructuredBinary (
    -- * Encoding and decoding
    -- | These functions operate like @binary@'s counterparts,
    -- but the serialised version has a structure hash in front.
    structuredEncode,
    structuredDecode,
    structuredDecodeOrFailIO,
    -- * Structured class
    Structured (structure),
    MD5,
    structureHash,
    genericStructure,
    GStructured,
    nominalStructure,
    containerStructure,
    -- * Structure type
    Structure (..),
    TypeName,
    ConstructorName,
    TypeVersion,
    SopStructure,
    hashStructure,
    typeVersion,
    typeName,
    ) where

import Data.Int                           (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty                 (NonEmpty)
import Data.Proxy                         (Proxy (..))
import Data.Ratio                         (Ratio)
import Data.Tagged                        (Tagged (..), untag)
import Data.Typeable                      (Typeable)
import Data.Word                          (Word, Word16, Word32, Word64, Word8)

import Control.Exception (catch, evaluate)
#if __GLASGOW_HASKELL__ >= 711
import Control.Exception (pattern ErrorCall)
#else
import Control.Exception (ErrorCall(..))
#endif

import GHC.Generics

import qualified Distribution.Compat.Binary as Binary
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntMap          as IM
import qualified Data.IntSet          as IS
import qualified Data.Map             as Map
import qualified Data.Sequence        as Seq
import qualified Data.Set             as Set
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as LT
import qualified Data.Time            as Time

#ifdef MIN_VERSION_aeson
import qualified Data.Aeson as Aeson
#endif

#if __GLASGOW_HASKELL__ >= 800
import Data.Kind (Type)
#else
#define Type *
#endif

#if MIN_VERSION_base(4,7,0)
import Data.Typeable (typeRep)
#else
import Data.Typeable (TypeRep, Typeable1, typeOf, typeOf1)
#endif

import Distribution.Compat.MD5

-------------------------------------------------------------------------------
-- Compat
-------------------------------------------------------------------------------

#if !MIN_VERSION_base(4,7,0)
typeRep :: forall a proxy. Typeable a => proxy a -> TypeRep
typeRep _ = typeOf (undefined :: a)
#endif

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type TypeName        = String
type ConstructorName = String

-- | A sematic version of a data type. Usually 0.
type TypeVersion     = Word32

-- | Structure of a datatype
data Structure
    = Nominal   !TypeVersion TypeName [Structure]  -- ^ nominal, yet can be parametrised by other structures.
    | Newtype   !TypeVersion TypeName Structure    -- ^ a newtype wrapper
    | Structure !TypeVersion TypeName SopStructure -- ^ sum-of-products structure
  deriving (Eq, Ord, Show, Generic)

instance Binary.Binary Structure
instance Structured Structure

type SopStructure = [(ConstructorName, [Structure])]

-- | A MD5 hash digest of 'Structure'.
hashStructure :: Structure -> MD5
hashStructure = md5 . LBS.toStrict . Binary.encode

-- | A van-Laarhoven lens into 'TypeVersion' of 'Structure'
--
-- @
-- 'typeVersion' :: Lens' 'Structure' 'TypeVersion'
-- @
typeVersion :: Functor f => (TypeVersion -> f TypeVersion) -> Structure -> f Structure
typeVersion f (Nominal v n s)   = fmap (\v' -> Nominal v' n s) (f v)
typeVersion f (Newtype v n s)   = fmap (\v' -> Newtype v' n s) (f v)
typeVersion f (Structure v n s) = fmap (\v' -> Structure v' n s) (f v)

-- | A van-Laarhoven lens into 'TypeName' of 'Structure'
--
-- @
-- 'typeName' :: Lens' 'Structure' 'TypeName'
-- @
typeName :: Functor f => (TypeName -> f TypeName) -> Structure -> f Structure
typeName f (Nominal v n s)   = fmap (\n' -> Nominal v n' s) (f n)
typeName f (Newtype v n s)   = fmap (\n' -> Newtype v n' s) (f n)
typeName f (Structure v n s) = fmap (\n' -> Structure v n' s) (f n)

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Class of types with a known 'Structure'.
--
-- For regular non-recursive data types 'Structured' can be derived generically.
--
-- @
-- data Record = Record { a :: Int, b :: Bool, c :: [Char] } deriving ('Generic')
-- instance 'Structured' Record
-- @
--
-- For other types check 'nominalStructure', 'containerStructure' etc.
--
class Structured a where
    structure :: Proxy a -> Structure
    default structure :: (Generic a, GStructured (Rep a)) => Proxy a -> Structure
    structure = genericStructure

    -- This member is hidden. It's there to precalc
    structureHash' :: Tagged a MD5
    structureHash' = Tagged (hashStructure (structure (Proxy :: Proxy a)))


-- | Semantically @'hashStructure' . 'structure'@.
structureHash :: forall a. Structured a => Proxy a -> MD5
structureHash _ = untag (structureHash' :: Tagged a MD5)

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- | Structured 'Binary.encode'.
-- Encode a value to using binary serialisation to a lazy 'LBS.ByteString'.
-- Encoding starts with 20 byte large structure hash.
structuredEncode
  :: forall a. (Binary.Binary a, Structured a)
  => a -> LBS.ByteString
structuredEncode x = Binary.encode (Tag :: Tag a, x)

-- | Structured 'Binary.decode'.
-- Decode a value from a lazy 'LBS.ByteString', reconstructing the original structure.
-- Throws pure exception on invalid inputs.
structuredDecode
  :: forall a. (Binary.Binary a, Structured a)
  => LBS.ByteString -> a
structuredDecode lbs = snd (Binary.decode lbs :: (Tag a, a))

structuredDecodeOrFailIO :: (Binary.Binary a, Structured a) => LBS.ByteString -> IO (Either String a)
structuredDecodeOrFailIO bs =
  catch (evaluate (structuredDecode bs) >>= return . Right)
  $ \(ErrorCall str) -> return $ Left str

-------------------------------------------------------------------------------
-- Helper data
-------------------------------------------------------------------------------

data Tag a = Tag

instance Structured a => Binary.Binary (Tag a) where
    get = do
        actual <- binaryGetMD5
        if actual == expected
        then return Tag
        else fail $ concat
            [ "Non-matching structured hashes: "
            , showMD5 actual
            , "; expected: "
            , showMD5 expected
            ]
      where
        expected = untag (structureHash' :: Tagged a MD5)

    put _ = binaryPutMD5 expected
      where
        expected = untag (structureHash' :: Tagged a MD5)

-------------------------------------------------------------------------------
-- Smart constructors
-------------------------------------------------------------------------------

-- | Use 'Typeable' to infer name
nominalStructure :: Typeable a => Proxy a -> Structure
nominalStructure p = Nominal 0 (show (typeRep p)) []

#if MIN_VERSION_base(4,7,0)
containerStructure :: forall f a. (Typeable f, Structured a) => Proxy (f a) -> Structure
containerStructure _ = Nominal 0 (show (typeRep (Proxy :: Proxy f)))
    [ structure (Proxy :: Proxy a)
    ]
#else
containerStructure :: forall f a. (Typeable1 f, Structured a) => Proxy (f a) -> Structure
containerStructure _ = Nominal 0 (show (typeOf1 (undefined :: f ())))
    [ structure (Proxy :: Proxy a)
    ]
#endif

-------------------------------------------------------------------------------
-- Generic
-------------------------------------------------------------------------------

-- | Derive 'structure' genrically.
genericStructure :: forall a. (Generic a, GStructured (Rep a)) => Proxy a -> Structure
genericStructure _ = gstructured (Proxy :: Proxy (Rep a)) 0

-- | Used to implement 'genericStructure'.
class GStructured (f :: Type -> Type) where
    gstructured :: Proxy f -> TypeVersion -> Structure

instance (i ~ D, Datatype c, GStructuredSum f) => GStructured (M1 i c f) where
    gstructured _ v = case sop of
#if MIN_VERSION_base(4,7,0)
        [(_, [s])] | isNewtype p -> Newtype v name s
#endif
        _                        -> Structure v name sop
      where
        p    = undefined :: M1 i c f ()
        name = datatypeName p
        sop  = gstructuredSum (Proxy :: Proxy f) []

class GStructuredSum (f :: Type -> Type) where
    gstructuredSum :: Proxy f -> SopStructure -> SopStructure

instance (i ~ C, Constructor c, GStructuredProd f) => GStructuredSum (M1 i c f) where
    gstructuredSum _ xs = (name, prod) : xs
      where
        name = conName (undefined :: M1 i c f ())
        prod = gstructuredProd (Proxy :: Proxy f) []

instance (GStructuredSum f, GStructuredSum g) => GStructuredSum (f :+: g) where
    gstructuredSum _ xs
        = gstructuredSum (Proxy :: Proxy f)
        $ gstructuredSum (Proxy :: Proxy g) xs

instance GStructuredSum V1 where
    gstructuredSum _ = id

class GStructuredProd (f :: Type -> Type) where
    gstructuredProd :: Proxy f -> [Structure] -> [Structure]

instance (i ~ S, GStructuredProd f) => GStructuredProd (M1 i c f) where
    gstructuredProd _ = gstructuredProd (Proxy :: Proxy f)

instance Structured c => GStructuredProd (K1 i c) where
    gstructuredProd _ xs = structure (Proxy :: Proxy c) : xs

instance GStructuredProd U1 where
    gstructuredProd _ = id

instance (GStructuredProd f, GStructuredProd g) => GStructuredProd (f :*: g) where
    gstructuredProd _ xs
        = gstructuredProd (Proxy :: Proxy f)
        $ gstructuredProd (Proxy :: Proxy g) xs

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

instance Structured ()
instance Structured Bool
instance Structured Ordering

instance Structured Char    where structure = nominalStructure
instance Structured Int     where structure = nominalStructure
instance Structured Integer where structure = nominalStructure

instance Structured Data.Word.Word where structure = nominalStructure

instance Structured Int8  where structure = nominalStructure
instance Structured Int16 where structure = nominalStructure
instance Structured Int32 where structure = nominalStructure
instance Structured Int64 where structure = nominalStructure

instance Structured Word8  where structure = nominalStructure
instance Structured Word16 where structure = nominalStructure
instance Structured Word32 where structure = nominalStructure
instance Structured Word64 where structure = nominalStructure

instance Structured Float  where structure = nominalStructure
instance Structured Double where structure = nominalStructure

instance Structured a => Structured (Maybe a)
instance (Structured a, Structured b) => Structured (Either a b)
instance Structured a => Structured (Ratio a) where structure = containerStructure
instance Structured a => Structured [a] where structure = containerStructure
instance Structured a => Structured (NonEmpty a) where structure = containerStructure

instance (Structured a1, Structured a2) => Structured (a1, a2)
instance (Structured a1, Structured a2, Structured a3) => Structured (a1, a2, a3)
instance (Structured a1, Structured a2, Structured a3, Structured a4) => Structured (a1, a2, a3, a4)
instance (Structured a1, Structured a2, Structured a3, Structured a4, Structured a5) => Structured (a1, a2, a3, a4, a5)
instance (Structured a1, Structured a2, Structured a3, Structured a4, Structured a5, Structured a6) => Structured (a1, a2, a3, a4, a5, a6)
instance (Structured a1, Structured a2, Structured a3, Structured a4, Structured a5, Structured a6, Structured a7) => Structured (a1, a2, a3, a4, a5, a6, a7)

instance Structured BS.ByteString where structure = nominalStructure
instance Structured LBS.ByteString where structure = nominalStructure

instance Structured T.Text where structure = nominalStructure
instance Structured LT.Text where structure = nominalStructure

instance (Structured k, Structured v) => Structured (Map.Map k v) where structure _ = Nominal 0 "Map" [ structure (Proxy :: Proxy k), structure (Proxy :: Proxy v) ]
instance (Structured k) => Structured (Set.Set k) where structure = containerStructure
instance (Structured v) => Structured (IM.IntMap v) where structure = containerStructure
instance Structured IS.IntSet where structure = nominalStructure
instance (Structured v) => Structured (Seq.Seq v) where structure = containerStructure

instance Structured Time.UTCTime         where structure = nominalStructure
instance Structured Time.DiffTime        where structure = nominalStructure
instance Structured Time.UniversalTime   where structure = nominalStructure
instance Structured Time.NominalDiffTime where structure = nominalStructure
instance Structured Time.Day             where structure = nominalStructure
instance Structured Time.TimeZone        where structure = nominalStructure
instance Structured Time.TimeOfDay       where structure = nominalStructure
instance Structured Time.LocalTime       where structure = nominalStructure

instance Structured (Proxy a)
instance Structured a => Structured (Tagged b a)
