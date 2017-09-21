{-# LANGUAGE
    TemplateHaskell,
    RankNTypes
    #-}
module Dota.Hero where

import Lens.Micro
import Lens.Micro.TH

import Data.Vector (Vector)
import qualified Data.Vector as V

import Dota.Ability

-- An attribute's base value and growth per level
data GrowingAttribute = !Double :++ !Double deriving (Eq, Ord, Show)
attrBase, attrGrowth :: Lens' GrowingAttribute Double
attrBase   f (b :++ g) = (:++ g) <$> f b
attrGrowth f (b :++ g) = (b :++) <$> f g

data AttackCapability = None | Ranged | Melee deriving (Eq, Ord, Enum, Bounded, Show)

data Attribute = Strength | Agility | Intelligence deriving (Eq, Ord, Enum, Bounded, Show)

data Hero = Hero {
    _heroStrength :: GrowingAttribute,
    _heroAgility :: GrowingAttribute,
    _heroIntelligence :: GrowingAttribute,
    _heroAttackCap :: AttackCapability,
    _heroAttackRange :: Int,
    _heroAttackProjectileSpeed :: Int,
    _heroAttackTime :: Double,
    _heroAttackBaseDamage :: (Int, Int),
    _heroAbilities :: Vector Ability
    } deriving (Eq, Ord, Show)
makeLenses ''Hero

heroAttr :: Attribute -> Lens' Hero GrowingAttribute
heroAttr Strength = heroStrength
heroAttr Agility = heroAgility
heroAttr Intelligence = heroIntelligence

-- | Calculate the total attribute value at a given level
attrTotal :: Int -> GrowingAttribute -> Double
attrTotal lv (base :++ growth) = base + growth * (fromIntegral lv - 1)