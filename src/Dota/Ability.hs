{-# LANGUAGE
    TemplateHaskell,
    DeriveGeneric
    #-}
{- | This module defines datatypes for Dota-style abilities
    and talents (which are a special kind of ability).
-}
module Dota.Ability where

import Lens.Micro
import Lens.Micro.TH

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Map (Map)
import qualified Data.Map as M

import GHC.Generics

import Data.Map.Ordered (OrderedMap)
import qualified Data.Map.Ordered as O

-- | The ability's BaseClass. This is generally only set explicitly when reading KV files,
--  and otherwise defaults to @AbilityLua Nothing@.
data AbilityBase = AbilityKV | AbilityLua !(Maybe String) deriving (Eq, Ord, Show, Generic)

-- | A value that changes with an ability's levels (or not). AFAIK, Dota only allows these to be
--  floating or integer, so these are the only supported types.
data LevelingVal = ValFloat !(Vector Double)
                | ValInt !(Vector Int)
                deriving (Eq, Ord, Show, Generic)

-- | Construct a LevelingVal from a vector of internal values.
-- Returns @Nothing@ if passed an empty @Vector a@.
fromValues :: IsLevelingVal a =>  Vector a -> Maybe LevelingVal
fromValues v = if V.null v then Nothing else fromValues_ v

-- | The class of values that can be converted to and from a LevelingVal.
class IsLevelingVal a where
    -- | Extract a LevelingVal's internal vector. Returns @Nothing@ if the
    -- LevelingVal's type does not match @a@.
    toValues :: LevelingVal -> Maybe (Vector a)
    -- | Construct a LevelingVal from a vector of internal values. Does not check
    -- for empty vectors (and may thus create invalid @LevelingVal@s). This method is not exported.
    fromValues_ :: Vector a -> LevelingVal

instance IsLevelingVal Int where
    toValues (ValInt vals) = Just vals
    toValues (ValFloat _) = Nothing
    fromValues_ = ValInt

instance IsLevelingVal Double where
    toValues (ValFloat vals) = Just vals
    toValues (ValInt _) = Nothing
    fromValues_ = ValFloat

type NamedLevelingVals = OrderedMap String LevelingVal

mkNamedLevelingVals :: (Functor f, Foldable f) => Vector (String, LevelingVal) -> NamedLevelingVals
mkNamedLevelingVals vals = NamedLevelingVals (fmap fst vals) (foldr (uncurry M.insert) M.empty vals)

-- | A Dota-style ability.
data Ability = Ability {
    -- | The ability's (English) name.
    _aName :: String,
    -- | The ability's internal ID. This is derived from its name, unless the ability was read from a KV file.
    _aID :: String,
    -- | The ability's BaseClass (see 'AbilityBase' definition)
    _aBase :: AbilityBase,
    -- | The levels at which the ability can be upgraded. The length of this vector determines the ability's maximum level.
    _aCanLevelAt :: Vector Int,
    -- | The ability's associated values (e.g. damage, duration, radius).
    _aLevelingValues :: NamedLevelingVals
    } deriving (Eq, Ord, Show, Generic)
makeLenses ''Ability

-- | A talent. This is a special type of ability that has only one level and appears in a hero's talent panel
-- rather than the regular ability list.
data Talent = Talent {
    -- | Some talents have an associated value, which can be integral or floating.
    -- e.g. a Strength talent has to specify how much Strength it grants.
    _tVal :: Maybe (Either Double Int),
    -- | The talent's (English) name. This also serves as the description.
    _tName :: String,
    -- | The talent's internal ID. This field is only set or read when working with KV files.
    _tID :: Maybe String
    } deriving (Eq, Ord, Show, Generic)
makeLenses ''Talent

-- | A pair of talents. At certain levels, heroes may choose either the left or right talent of a talent pair.
data TalentPair = TalentPair {
    _leftTalent :: Talent,
    _rightTalent :: Talent
    } deriving (Eq, Ord, Show, Generic)
makeLenses ''TalentPair

-- | The levels at which talents can be upgraded. These are currently [10, 15, 20, 25].
talentLevels :: Vector Int
talentLevels = V.fromList [10, 15, 20, 25]

type Talents = Vector TalentPair