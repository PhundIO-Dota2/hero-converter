{-# LANGUAGE
    RecordWildCards,
    BangPatterns
    #-}
{- | This module contains instances and functions to de/serialize a hero's internal representation
    as JSON.
-}
module Dota.Hero.Format.Native.JSON where

import Lens.Micro

import Data.Aeson

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Monoid
import Control.Applicative

import Dota.Ability
import Dota.Hero

instance FromJSON Ability
instance ToJSON Ability

instance FromJSON Hero
instance ToJSON Hero