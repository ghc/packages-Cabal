{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.FieldGrammar.Described (
    Described (..),
    ) where

import Distribution.Parsec           (Parsec)
import Distribution.Pretty           (Pretty)

import Data.Functor.Identity (Identity (..))

import qualified Text.PrettyPrint as PP

-- | Class describing the pretty/parsec format of a.
class (Pretty a, Parsec a) => Described a where
    -- | A pretty document of "regex" describing the field format
    --
    -- TODO: use ADT for a "regex" type (to avoid different formats)
    describe :: proxy a -> PP.Doc

instance Described a => Described (Identity a) where
    describe _ = describe ([] :: [a])
