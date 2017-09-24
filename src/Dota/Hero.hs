{-# LANGUAGE
    TemplateHaskell,
    RankNTypes
    #-}
{- | Core data definitions for a Dota-style hero.

-}
module Dota.Hero where

import Lens.Micro
import Lens.Micro.TH

import Data.Vector (Vector)
import qualified Data.Vector as V

import GHC.Generics

import Data.Monoid

import Dota.Ability

-- | An attribute's base value and growth per level
data GrowingAttribute = !Double :++ !Double deriving (Eq, Ord, Show, Generic)
attrBase, attrGrowth :: Lens' GrowingAttribute Double
attrBase   f (b :++ g) = (:++ g) <$> f b
attrGrowth f (b :++ g) = (b :++) <$> f g

-- | A unit's attack capability. This reflects the valid values in Dota.
data AttackCapability = None | Ranged | Melee deriving (Eq, Ord, Enum, Bounded, Show, Generic)

-- | One of Dota's three main attributes: Strength, Agility, or Intelligence.
data Attribute = Strength | Agility | Intelligence deriving (Eq, Ord, Enum, Bounded, Show, Generic)

-- | A full hero description
data Hero = Hero {
    _heroPrimaryAtttribute :: Attribute,
    _heroStrength :: GrowingAttribute,
    _heroAgility :: GrowingAttribute,
    _heroIntelligence :: GrowingAttribute,
    _heroAttackCap :: AttackCapability,
    _heroAttackRange :: Int,
    _heroAttackProjectileSpeed :: Int,
    _heroAttackTime :: Double,
    _heroAttackBaseDamage :: (Int, Int),
    _heroAbilities :: Vector Ability,
    _heroTalents :: Talents
    } deriving (Eq, Ord, Show, Generic)
makeLenses ''Hero

-- | Convenience "indexed" lens for a hero's attribute.
heroAttr :: Attribute -> Lens' Hero GrowingAttribute
heroAttr Strength = heroStrength
heroAttr Agility = heroAgility
heroAttr Intelligence = heroIntelligence

-- | Convenience lens to directly access a hero's main attribute
heroMainAttr :: Lens' Hero GrowingAttribute
heroMainAttr f h = case h ^. heroPrimaryAtttribute of
    Strength -> heroStrength f h
    Agility -> heroAgility f h
    Intelligence -> heroIntelligence f h

-- | Calculate the total attribute value at a given level
attrTotal :: Int -> GrowingAttribute -> Double
attrTotal lv (base :++ growth) = base + growth * (fromIntegral lv - 1)