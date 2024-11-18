module KoiKoi.Score (score) where

import Cards
import KoiKoi.Game

score :: Pile -> Int
score pile =
  scoreHikari cards
    + scoreKasu cards
    + scoreSpecials cards
    + scoreTane cards
    + scoreTan cards
  where
    cards = pileCards pile

scoreHikari :: [Card] -> Int
scoreHikari cards = case (length hikari, raining) of
  (5, _) -> 10
  (4, False) -> 8
  (4, True) -> 7
  (3, False) -> 5
  _ -> 0
  where
    hikari = withRank Hikari cards
    raining = any (hasName Michikaze) hikari

scoreTane :: [Card] -> Int
scoreTane cards
  | inoshikachou = 5 + tane - 3
  | tane >= 5 = tane - 4
  | otherwise = 0
  where
    tane = length . withRank Tane $ cards
    inoshikachou =
      elemName Yamajishi cards
        && elemName Shika cards
        && elemName Chou cards

scoreTan :: [Card] -> Int
scoreTan cards
  | akatan == 3 && aotan == 3 = 10 + tanzaku
  | akatan == 3 = 5 + (aotan + tanzaku)
  | aotan == 3 = 5 + (akatan + tanzaku)
  | anyTan >= 5 = anyTan - 4
  | otherwise = 0
  where
    count = length . flip withRank cards
    akatan = count Akatan
    aotan = count Aotan
    tanzaku = count Tanzaku
    anyTan = akatan + aotan + tanzaku

scoreKasu :: [Card] -> Int
scoreKasu cards = if kasu >= 10 then kasu - 9 else 0
  where
    kasu = length . withRank Kasu $ cards

scoreSpecials :: [Card] -> Int
scoreSpecials cards =
  (if sakura && sake then 5 else 0)
    + (if tsuki && sake then 5 else 0)
  where
    sakura = elemName Mankai cards
    tsuki = elemName Mochizuki cards
    sake = elemName Sakazuki cards
