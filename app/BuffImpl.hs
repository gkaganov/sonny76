{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module BuffImpl where

import Buff
import Hero

import Data.Sequence (Seq)

trigger :: Buff -> HeroID -> HeroID -> Seq Hero -> Seq Hero
trigger Poisoned caster target heroes = heroes
trigger Silenced caster target heroes = heroes
