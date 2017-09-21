{-# LANGUAGE
    TemplateHaskell
    #-}
module Dota.Ability where

import Lens.Micro
import Lens.Micro.TH

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Map (Map)
import qualified Data.Map as M

data AbilityBase = AbilityKV | AbilityLua !(Maybe String) deriving (Eq, Ord, Show)

data SpecialVal = ValFloat !(Values Double)
                | ValInt !(Values Int)
                deriving (Eq, Ord, Show)

class IsSpecialVal a where
    toValues :: SpecialVal -> Maybe (Values a)
    fromValues :: Values a -> SpecialVal

instance IsSpecialVal Int where
    toValues (ValInt vals) = Just vals
    toValues (ValFloat _) = Nothing
    fromValues = ValInt

instance IsSpecialVal Double where
    toValues (ValFloat vals) = Just vals
    toValues (ValInt _) = Nothing
    fromValues = ValFloat

data Values a = Constant !a | Varying !(Vector a) deriving (Eq, Ord, Show)

instance Functor Values where
    fmap f (Constant x) = Constant (f x)
    fmap f (Varying xs) = Varying (fmap f xs)

instance Foldable Values where
    foldMap f (Constant x) = f x
    foldMap f (Varying xs) = foldMap f xs

instance Traversable Values where
    traverse f (Constant x) = Constant <$> f x
    traverse f (Varying xs) = Varying <$> traverse f xs

data Ability = Ability {
    _aName :: String,
    _aBase :: AbilityBase,
    _aMaxLevel :: Int,
    _aCanLevelAt :: Vector Int,
    _aSpecialValues :: Map String SpecialVal
    } deriving (Eq, Ord, Show)
makeLenses ''Ability