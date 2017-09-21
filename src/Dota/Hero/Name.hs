{-# LANGUAGE
    OverloadedLists,
    OverloadedStrings,
    RecordWildCards,
    TypeApplications
    #-}
-- | Contains the various names and IDs of the game's default heroes.
module Dota.Hero.Name where

import Data.Set (Set)
import qualified Data.Set as S

import Data.Function
import Data.Maybe
import Data.String
import Data.Char
import Data.Bool
import Data.List

-- | A fully-specified hero name.
data HeroName = HN {
    hnCustom :: Bool,           -- ^ is this a custom hero?
    hnNameStrategy :: StandardName, -- ^ the name normally used to refer to this hero. Can be UseName, UseTitle, or an entirely different name.
    _hnID :: String,            -- ^ the suffix of hero's internal ID, e.g. @"zuus"@ for Zeus
    hnName :: String,           -- ^ the hero's name, e.g. @"Zeus"@ for Zeus or @"Rylai"@ for Crystal Maiden
    hnTitle :: Title            -- ^ the hero's title, e.g. @the "Lord of Heaven"@ for Zeus or @the "Crystal Maiden"@ for Crystal Maiden
    }

-- | Extract a hero's full ID from its _hnID field. 
--   This is done by prefixing it with "npc_dota_hero_" for default heroes, or "npc_dota_custom_hero_" for custom ones.
hnID :: HeroName -> String
hnID hn = bool "npc_dota_hero_" "npc_dota_custom_hero_" (hnCustom hn) ++ _hnID hn

-- | A hero's primary name (the name by which they're usually referred to)
hnPrimaryName :: HeroName -> String
hnPrimaryName hn = case hnNameStrategy hn of
    UseTitle -> titleBase $ hnTitle hn
    UseName -> hnName hn
    Other n -> n

-- | A hero's full name, e.g. "Lina, the Slayer"
hnFullName :: HeroName -> String
hnFullName HN{..} = if hnTitle == noTitle
    then hnName
    else hnName ++ ", " ++ show hnTitle

-- | Read a hero's full name from a string of the form "Name, the Title".
--   If the title is omitted, it will 
readUsing :: StandardName -> String -> HeroName
readUsing sn str = HN False sn iid name title
      where
        (name, rest) = span (/= ',') str
        hasTitle = not (null rest)
        title = if hasTitle
            then fromString $ fromMaybe "" $ stripPrefix ", " rest
            else noTitle
        iid = map conv $ case sn of
            UseName -> name
            UseTitle -> if hasTitle
                then titleBase title
                else name
            Other s -> s
        conv ' ' = '_'
        conv c = toLower c

instance Eq HeroName where (==) = (==) `on` _hnID
instance Ord HeroName where compare = compare `on` _hnID
instance Show HeroName where
    show hn = concat ([
        case hnNameStrategy hn of
            UseName -> hnName hn
            UseTitle -> titleBase $ hnTitle hn
            Other n -> n,
        " | ",
        hnName hn,
        if noTitle == hnTitle hn then "" else ", " ++ show (hnTitle hn),
        " (",
        hnID hn,
        ")"] ::[String]) -- GHC apparently doesn't default this to [] in the presence of OverloadedLists

data StandardName = UseTitle | UseName | Other !String deriving (Eq, Ord, Show)

instance IsString StandardName where fromString = Other

-- | A hero's title, e.g. "Zeus, /the Lord of Heaven/"
data Title = Title {
    titleHasPrefix :: Bool, -- ^ whether this title is prefixed by "the"
    titleBase :: String -- ^ the actual title
    }

instance Eq Title where (==) = (==) `on` titleBase
instance Ord Title where compare = compare `on` titleBase
instance Show Title where
    show t = if noTitle == t
        then ""
        else if titleHasPrefix t
            then "the " ++ titleBase t
            else titleBase t
instance IsString Title where
    fromString ('t':'h':'e':' ':t) = Title True t
    fromString t = Title False t

the :: String -> Title
the = Title True

noTitle :: Title
noTitle = Title False ""

findHeroName :: String -> Set HeroName -> Maybe HeroName
findHeroName needle hay = snd . S.foldr ((maxOn fst) . (\x -> (scoreMatch x, Just x))) (0, Nothing) $ hay
  where
    scoreMatch :: HeroName -> Int
    scoreMatch hn@HN{..} = if name ~= needle || hnName ~= needle || fullName ~= needle || show hnTitle ~= needle || _hnID ~= needle
        then maxBound
        else 0 -- TODO put an actual algorithm here
      where
        name = hnPrimaryName hn
        fullName = hnFullName hn
    maxOn f a b = if f a <= f b then b else a
    x ~= y = canon x == canon y
    canon = stripPrefix' "the" . map toLower . filter isLetter
    stripPrefix' p s = case stripPrefix p s of Nothing -> s; Just s' -> s'

defaultHeroes :: Set HeroName
defaultHeroes = [
    n "Abaddon, the Lord of Avernus",
    t "Razzil Darkbrew, the Alchemist",
    HN False UseName "antimage" "Anti-Mage" "",
    t "Kaldr, the Ancient Apparition",
    HN False UseTitle "arcwarden" "Zet" "the Arc Warden",
    t "Mogul Khan, the Axe",
    HN False "Bane" "bane" "Atropos" "the Bane Elemental",
    n "Batrider",
    t "Karroch, the Beastmaster",
    t "Strygwyr, the Bloodseeker",
    t "Gondar, the Bounty Hunter",
    t "Mangix, the Brewmaster",
    t "Rigwarl, the Bristleback",
    t "Black Arachnia, the Broodmother",
    HN False UseTitle "centaur" "Bradwarden" "the Centaur Warrunner",
    n "Chaos Knight",
    n "Chen, the Holy Knight",
    n "Clinkz, the Bone Fletcher",
    t "Rattletrap, the Clockwerk",
    t "Rylai, the Crystal Maiden",
    t "Ish'kafel, the Dark Seer",
    n "Dazzle, the Shadow Priest",
    t "Krobelus, the Death Prophet",
    n "Disruptor, the Stormcrafter",
    HN False UseTitle "doom_bringer" "Lucifer" "the Doom",
    t "Davion, the Dragon Knight",
    t "Traxex, the Drow Ranger",
    t "Kaolin, the Earth Spirit",
    t "Raigor Stonehoof, the Earthshaker",
    t ", the Elder Titan",
    t "Xin, the Ember Spirit",
    t "Aiushtha, the Enchantress",
    n "Enigma",
    t "Darkterror, the Faceless Void",
    t "Aurel, the Gyrocopter",
    n "Huskar, the Sacred Warrior",
    n "Invoker",
    t "Io, the Wisp",
    n "Jakiro, the Twin Head Dragon",
    t "Yurnero, the Juggernaut",
    t "Ezalor, the Keeper of the Light",
    n "Kunkka, the Admiral",
    t "Tresdin, the Legion Commander",
    n "Leshrac, the Tormented Soul",
    t "Ethreain, the Lich",
    HN False UseTitle "life_stealer" "N'aix" "the Lifestealer",
    n "Lina, the Slayer",
    n "Lion, the Demon Witch",
    t "Sylla, the Lone Druid",
    n "Luna, the Moon Rider",
    n "Banehallow, the Lycan",
    HN False UseName "magnataur" "Magnus" "the Magnoceros",
    n "Medusa, the Gorgon",
    n "Meepo, the Geomancer",
    n "Mirana, the Princess of the Moon",
    n "Morphling",
    t "Sun Wukong, the Monkey King",
    t "Slithice, the Naga Siren",
    HN False UseName "furion" "Nature's Prophet" "",
    HN False UseTitle "necrolyte" "Rotund'jere" "the Necrophos",
    t "Balanar, the Night Stalker",
    n "Nyx Assassin",
    t "Aggron Stonebreak, the Ogre Magi",
    t "Purist Thunderwrath, the Omniknight",
    t "Nerif, the Oracle",
    HN False UseTitle "obsidian_destroyer" "Harbinger" "the Outworld Devourer",
    t "Mortred, the Phantom Assassin",
    t "Azwraith, the Phantom Lancer",
    n "Phoenix",
    n "Puck, the Faerie Dragon",
    n "Pudge, the Butcher",
    n "Pugna, the Oblivion",
    HN False UseTitle "queenofpain" "Akasha" "the Queen of Pain",
    n "Razor, the Lightning Revenant",
    n "Riki, the Stealth Assassin",
    n "Rubick, the Grand Magus",
    t "Crixalis, the Sand King",
    n "Shadow Demon",
    n "Nevermore, the Shadow Fiend",
    t "Rhasta, the Shadow Shaman",
    t "Nortrom, the Silencer",
    t "Dragonus, the Skywrath Mage",
    n "Slardar, the Slithereen Guard",
    n "Slark, the Nightcrawler",
    t "Kardel Sharpeye, the Sniper",
    t "Mercurial, the Spectre",
    t "Barathrum, the Spirit Breaker",
    t "Raijin Thunderkeg, the Storm Spirit",
    n "Sven, the Rogue Knight",
    HN False UseTitle "techies" "Squee, Spleen and Spoon" "the Techies",
    t "Lanaya, the Templar Assassin",
    t "Terrorblade, the Demon Marauder",
    t "Leviathan, the Tidehunter",
    HN False UseTitle "shredder" "Razzrick" "the Timbersaw",
    t "Boush, the Tinker",
    n "Tiny, the Stone Giant",
    HN False UseTitle "treant" "Rooftrellen" "the Treant Protector",
    t "Jah'rakal, the Troll Warlord",
    t "Ymir, the Tusk",
    HN False UseTitle "abyssal_underlord" "Vrogros" "the Underlord",
    n "Undying, the Almighty Dirge",
    o "Ursa" "Ulfsaar, the Ursa Warrior",
    HN False UseTitle "vengefulspirit" "Shendelzare" "the Vengeful Spirit",
    t "Lesale Deathbringer, the Venomancer",
    n "Viper, the Netherdrake",
    n "Visage, the Bound Form of Necro'lic",
    t "Demnok Lannik, the Warlock",
    t "Skitskurr, the Weaver",
    HN False UseTitle "windrunner" "Lyralei" "the Windranger",
    t "Auroth, the Winter Wyvern",
    t "Zharvakko, the Witch Doctor",
    HN False UseTitle "skeleton_king" "Ostarion" "the Wraith King",
    HN False UseName "zuus" "Zeus" "the Lord of Heaven"]
  where
    n = readUsing UseName
    t = readUsing UseTitle
    o = readUsing . Other