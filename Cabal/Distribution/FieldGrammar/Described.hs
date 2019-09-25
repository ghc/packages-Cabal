{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.FieldGrammar.Described (
    Described (..),
    describeDoc,
    -- * Regular expressions
    Regex (..),
    reChar,
    reDigits,
    reDot,
    reMunchCS,
    reMunch1CS,
    CharSet (..),
    regexDoc,
    regexMatch,
    ) where

import Data.Char                   (isAlphaNum, isDigit)
import Data.String                 (IsString (..))
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec (Parsec)
import Distribution.Pretty (Pretty)

import Data.Functor.Identity (Identity (..))
import Data.Set              (Set)

import qualified Data.Set         as Set
import qualified Text.PrettyPrint as PP

-- | Class describing the pretty/parsec format of a.
class (Pretty a, Parsec a) => Described a where
    -- | A pretty document of "regex" describing the field format
    describe :: proxy a -> Regex

-- | Pretty-print description.
--
-- >>> describeDoc ([] :: [Bool])
-- True|False
describeDoc :: Described a => proxy a -> PP.Doc
describeDoc p = regexDoc (describe p)

instance Described Bool where
    describe _ = REUnion ["True", "False"]

instance Described a => Described (Identity a) where
    describe _ = describe ([] :: [a])

-------------------------------------------------------------------------------
-- Regex
-------------------------------------------------------------------------------

-- | Regular expressions tuned for 'Described' use-case.
data Regex
    = REAppend  [Regex] -- ^ append @ab@
    | REUnion   [Regex] -- ^ union @a|b@
    | REMunch   Regex   -- ^ star @a*@
    | REMunch1  Regex   -- ^ plus @a+@
    | REOpt     Regex   -- ^ optional @r?@
    | REString  String  -- ^ literal string @abcd@
    | RECharSet CharSet -- ^ charset @[:alnum:]@
    | RENamed   String
  deriving (Eq, Ord, Show)

reChar :: Char -> Regex
reChar = RECharSet . CSChar

reDigits :: Regex
reDigits = REMunch1 (RECharSet CSDigit)

reDot :: Regex
reDot = RECharSet (CSChar '.')

reMunch1CS :: CharSet -> Regex
reMunch1CS = REMunch1 . RECharSet

reMunchCS :: CharSet -> Regex
reMunchCS = REMunch . RECharSet

instance IsString Regex where
    fromString = REString

instance Semigroup Regex where
    x <> y = REAppend (unAppend x ++ unAppend y) where
        unAppend (REAppend rs) = rs
        unAppend r             = [r]

instance Monoid Regex where
    mempty = REAppend []
    mappend = (<>)

-- | Character sets.
--
-- Order of constructors is important.
-- @'CSDigit' < 'CSAlphaNum' < 'CSNotSpaceOrComma' < 'CSNotSpace'@
--
data CharSet
    = CSDigit               -- ^ decimal digits  @[:digit:]@
    | CSAlphaNum            -- ^ alpha-numeric   @[:alnum:]@
    | CSNotSpaceOrComma     -- ^ not space, nor comma: @[^ ,]@
    | CSNotSpace            -- ^ not space: @[^ ]@
    | CSChar Char           -- ^ single character
    | CSUnion (Set CharSet)
  deriving (Eq, Ord, Show)

-- | Union of 'CharSet's.
--
-- >>> CSAlphaNum <> CSDigit
-- CSAlphaNum
--
-- >>> CSAlphaNum <> CSChar '-'
-- CSUnion (fromList [CSAlphaNum,CSChar '-'])
--
instance Semigroup CharSet where
    x <> y = simplify1 $ Set.union (fromUnion x) (fromUnion y) where
        fromUnion (CSUnion s) = s
        fromUnion s           = Set.singleton s

        simplify1 s | Set.member CSAlphaNum s = simplify2 (removeChars isAlphaNum $ Set.filter (>= CSAlphaNum) s)
                    | otherwise               = simplify2 s

        simplify2 s | Set.member CSDigit s = simplify3 (removeChars isDigit s)
                    | otherwise            = simplify3 s

        removeChars p = Set.filter $ \z -> case z of
            CSChar c -> not (p c)
            _        -> True

        simplify3 s = case Set.toList s of
            [s'] -> s'
            _    -> CSUnion s

-- |
--
-- >>> regexDoc $ REString "True"
-- True
--
-- >>> regexDoc $ REString "foo" <> REString "bar"
-- foobar
--
-- >>> regexDoc $ REUnion [REString "False" , REString "True"]
-- False|True
--
-- >>> regexDoc $ REMunch1 $ RECharSet $ CSAlphaNum <> CSChar '-'
-- [[:alnum:]-]+
--
-- >>> regexDoc $ REMunch1 $ REUnion [ RECharSet $ CSAlphaNum <> CSChar '-', REString "weird"]
-- ([[:alnum:]-]|weird)+
--
-- >>> regexDoc $ RENamed "something"
-- {something}
--
regexDoc :: Regex -> PP.Doc
regexDoc = go 0 where
    go :: Int -> Regex -> PP.Doc
    go d (REAppend rs)  = parensIf (d > 1) $ PP.hcat (map (go 1) rs)
    go d (REUnion rs)   = parensIf (d > 2) $ PP.hcat (PP.punctuate (PP.char '|') (map (go 2) rs))
    go _ (REMunch r)    = go 3 r <<>> PP.char '*'
    go _ (REMunch1 r)   = go 3 r <<>> PP.char '+'
    go _ (REOpt r)      = go 3 r <<>> PP.char '?'
    go d (REString s)   = parensIf (d > 2) $ PP.text s
    go _ (RECharSet cs) = charsetDoc cs
    go _ (RENamed n)    = PP.braces (PP.text n)

    parensIf :: Bool -> PP.Doc -> PP.Doc
    parensIf True  = PP.parens
    parensIf False = id

-- |
--
-- >>> traverse_ (print . charsetDoc) [CSDigit, CSAlphaNum, CSNotSpaceOrComma, CSNotSpace, CSChar 'a']
-- [:digit:]
-- [:alnum:]
-- [^ ,]
-- [^ ]
-- a
--
-- >>> print $ charsetDoc $ CSAlphaNum <> CSChar '-'
-- [[:alnum:]-]
--
charsetDoc :: CharSet -> PP.Doc
charsetDoc CSDigit           = PP.text "[:digit:]"
charsetDoc CSAlphaNum        = PP.text "[:alnum:]"
charsetDoc CSNotSpaceOrComma = PP.text "[^ ,]"
charsetDoc CSNotSpace        = PP.text "[^ ]"
charsetDoc (CSChar c)        = PP.char c
charsetDoc (CSUnion cs)      = PP.brackets $ PP.hcat (map charsetDoc $ Set.toList cs)

regexMatch :: Regex -> String -> Bool
regexMatch _ _ = False
