{-# LANGUAGE
    RecordWildCards,
    BangPatterns
    #-}
module Dota.Hero.Format.Native where

import Lens.Micro

import Data.Binary

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Monoid
import Control.Applicative

import Dota.Ability
import Dota.Hero

-- | Convert a short vector to binary form. Length must be <= 255; this precondition is not checked.
putShortVec :: Binary a => Vector a -> Put
putShortVec v = putWord8 (fromIntegral $! length v) <> mapM_ put v

-- | Decode a short vector. Length will be <= 255.
getShortVec :: Binary a => Get (Vector a)
getShortVec = do
    len <- toInteger <$> getWord8
    V.reverse . V.fromList <$> go [] len
  where
    go !xs 0 = pure xs
    go !xs !len = do  
        x <- get
        x `seq` go (x:xs) (len-1)

instance Binary AbilityBase where
    put AbilityKV = putWord8 0
    put (AbilityLua Nothing) = putWord8 1
    put (AbilityLua (Just s)) = putWord8 2 <> put s
    get = do
        tag <- getWord8
        case tag of
            0 -> pure AbilityKV
            1 -> pure $! AbilityLua Nothing
            2 -> AbilityLua . Just <$> get

instance Binary LevelingVal where
    put (ValFloat fs) = putWord8 0 <> putShortVec fs
    put (ValInt is) = putWord8 1 <> putShortVec is
    get = do
        tag <- getWord8
        case tag of
            0 -> ValFloat <$> getShortVec
            1 -> ValInt <$> getShortVec

instance Binary Ability where
    put Ability{..} = mconcat [
        put _aName, put _aID,
        put _aBase,
        putShortVec _aCanLevelAt,
        put _aLevelingValues]
    get = Ability
        <$> get <*> get
        <*> get
        <*> getShortVec
        <*> get

instance Binary Talent where
    put Talent{..} = case _tVal of
        Nothing -> putWord8 0 <> put _tName
        Just (Left f) -> putWord8 1 <> put f <> put _tName
        Just (Right i) -> putWord8 2 <> put i <> put _tName
    get = do
        tag <- getWord8
        case tag of
            0 -> Talent Nothing <$> get
            1 -> Talent <$> (Just . Left <$> get) <*> get
            2 -> Talent <$> (Just . Right <$> get) <*> get

instance Binary TalentPair where
    put (TalentPair l r) = put l <> put r
    get = TalentPair <$> get <*> get

instance Binary GrowingAttribute where
    put (b :++ g) = put b <> put g
    get = liftA2 (:++) get get

instance Binary Attribute where
    put Strength = putWord8 0
    put Agility = putWord8 1
    put Intelligence = putWord8 2
    get = do
        i <- getWord8
        pure $ case i of
            0 -> Strength
            1 -> Agility
            2 -> Strength

instance Binary AttackCapability where
    put None = putWord8 0
    put Ranged = putWord8 1
    put Melee = putWord8 2
    get = do
        i <- getWord8
        pure $ case i of
            0 -> None
            1 -> Ranged
            2 -> Melee

instance Binary Hero where
    put Hero{..} = mconcat [
        put _heroPrimaryAtttribute,
        put _heroStrength, put _heroAgility, put _heroIntelligence,
        put _heroAttackCap, put _heroAttackRange, put _heroAttackProjectileSpeed,
        put _heroAttackTime, put _heroAttackBaseDamage,
        putShortVec _heroAbilities,
        putShortVec _heroTalents]
    get = Hero
        <$> get
        <*> get <*> get <*> get
        <*> get <*> get <*> get
        <*> get <*> get
        <*> getShortVec
        <*> getShortVec