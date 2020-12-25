{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Risk where

import Control.Monad.Random
import Data.Functor
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army}
  deriving (Show)

-- 1

-- 2

dies :: Integer -> Rand StdGen [DieValue]
dies n = replicateM (fromInteger n) die

battle :: Battlefield -> Rand StdGen Battlefield
battle Battlefield {attackers = att, defenders = def} =
  do
    att' <- dies attU
    def' <- dies defU
    let mix = zip' (sort att') (sort def')
        (att'', def'') = foldl addTup (att, def) $ map f mix
    return Battlefield {attackers = att'', defenders = def''}
  where
    attU = min 3 $ fromIntegral att - 1
    defU = min 2 $ fromIntegral def
    zip' x y = take (max (length x) (length y)) $ zip (x ++ repeat 0) (y ++ repeat 0)
    f (x, y) = if x > y then (0, -1) else (-1, 0)
    addTup (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

-- 3

invade :: Battlefield -> Rand StdGen Battlefield
invade bat@Battlefield {attackers = att, defenders = def} =
  if def <= 0 || att < 2
    then return bat
    else invade =<< battle bat

-- 4

successProb :: Battlefield -> Rand StdGen Double
successProb bat =
  replicateM 1000 (invade bat) <&> filter ((<= 0) . defenders)
    <&> \x -> fromIntegral (length x) / 1000

-- 5

exactSuccessProb :: Battlefield -> Double
exactSuccessProb = undefined
