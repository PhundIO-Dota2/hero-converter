{-# LANGUAGE
    TemplateHaskell,
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable
    #-}
module Dota.Ability where

import Lens.Micro
import Lens.Micro.TH

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Map (Map)
import qualified Data.Map as M

import Data.Binary

data AbilityBase = AbilityKV | AbilityLua !(Maybe String) deriving (Eq, Ord, Show)

data LevelingVal = ValFloat !(Vector Double)
                | ValInt !(Vector Int)
                deriving (Eq, Ord, Show)

class IsLevelingVal a where
    toValues :: LevelingVal -> Maybe (Vector a)
    fromValues :: Vector a -> LevelingVal

instance IsLevelingVal Int where
    toValues (ValInt vals) = Just vals
    toValues (ValFloat _) = Nothing
    fromValues = ValInt

instance IsLevelingVal Double where
    toValues (ValFloat vals) = Just vals
    toValues (ValInt _) = Nothing
    fromValues = ValFloat

data Ability = Ability {
    _aName :: String,
    _aID :: String,
    _aBase :: AbilityBase,
    _aCanLevelAt :: Vector Int,
    _aLevelingValues :: Map String LevelingVal
    } deriving (Eq, Ord, Show)
makeLenses ''Ability
    
data Talent = Talent {
    _tVal :: Maybe (Either Double Int),
    _tName :: String
    } deriving (Eq, Ord, Show)
makeLenses ''Talent

data TalentPair = TalentPair {
    _leftTalent :: Talent,
    _rightTalent :: Talent
    } deriving (Eq, Ord, Show)
makeLenses ''TalentPair

talentLevels :: Vector Int
talentLevels = V.fromList [10, 15, 20, 25]

type Talents = Vector TalentPair