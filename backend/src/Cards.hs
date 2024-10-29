module Cards
  ( Card,
    Suit (..),
    Rank (..),
    Name (..),
    deck,
    fromName,
    hasName,
    withRank,
    withSuit,
  )
where

data Suit
  = Matsu
  | Ume
  | Sakura
  | Fuji
  | Kakitsubata
  | Botan
  | Hagi
  | Susuki
  | Kiku
  | Momiji
  | Yanagi
  | Kiri
  deriving (Eq, Ord, Show)

data Rank = Hikari | Tane | Akatan | Aotan | Tanzaku | Kasu
  deriving (Eq, Ord, Show)

data Card
  = MatsuNiTsuru
  | MatsuNiAkatan
  | MatsuNoKasu1
  | MatsuNoKasu2
  | UmeNiUguisu
  | UmeNiAkatan
  | UmeNoKasu1
  | UmeNoKasu2
  | SakuraNiManmaku
  | SakuraNiAkatan
  | SakuraNoKasu1
  | SakuraNoKasu2
  | FujiNiJichou
  | FujiNiTanzaku
  | FujiNoKasu1
  | FujiNoKasu2
  | KakitsubataNiYatsuhashi
  | KakitsubataNiTanzaku
  | KakitsubataNoKasu1
  | KakitsubataNoKasu2
  | BotanNiChou
  | BotanNiAotan
  | BotanNoKasu1
  | BotanNoKasu2
  | HagiNiYamajishi
  | HagiNiTanzaku
  | HagiNoKasu1
  | HagiNoKasu2
  | SusukiNiMochizuki
  | SusukiNiKari
  | SusukiNoKasu1
  | SusukiNoKasu2
  | KikuNiSakazuki
  | KikuNiAotan
  | KikuNoKasu1
  | KikuNoKasu2
  | MomijiNiShika
  | MomijiNiAotan
  | MomijiNoKasu1
  | MomijiNoKasu2
  | YanagiNiMichikaze
  | YanagiNiTsubame
  | YanagiNiTanzaku
  | YanagiNoKasu
  | KiriNiHouou
  | KiriNoKasu1
  | KiriNoKasu2
  | KiriNoKasu3
  deriving (Eq, Ord, Show)

data Description = Description
  { card :: Card,
    suit :: Suit,
    rank :: Rank
  }

cards :: [Description]
cards =
  [ Description MatsuNiTsuru Matsu Hikari,
    Description MatsuNiAkatan Matsu Akatan,
    Description MatsuNoKasu1 Matsu Kasu,
    Description MatsuNoKasu2 Matsu Kasu,
    Description UmeNiUguisu Ume Tane,
    Description UmeNiAkatan Ume Akatan,
    Description UmeNoKasu1 Ume Kasu,
    Description UmeNoKasu2 Ume Kasu,
    Description SakuraNiManmaku Sakura Hikari,
    Description SakuraNiAkatan Sakura Akatan,
    Description SakuraNoKasu1 Sakura Kasu,
    Description SakuraNoKasu2 Sakura Kasu,
    Description FujiNiJichou Fuji Tane,
    Description FujiNiTanzaku Fuji Tanzaku,
    Description FujiNoKasu1 Fuji Kasu,
    Description FujiNoKasu2 Fuji Kasu,
    Description KakitsubataNiYatsuhashi Kakitsubata Tane,
    Description KakitsubataNiTanzaku Kakitsubata Tanzaku,
    Description KakitsubataNoKasu1 Kakitsubata Kasu,
    Description KakitsubataNoKasu2 Kakitsubata Kasu,
    Description BotanNiChou Botan Tane,
    Description BotanNiAotan Botan Aotan,
    Description BotanNoKasu1 Botan Kasu,
    Description BotanNoKasu2 Botan Kasu,
    Description HagiNiYamajishi Hagi Tane,
    Description HagiNiTanzaku Hagi Tanzaku,
    Description HagiNoKasu1 Hagi Kasu,
    Description HagiNoKasu2 Hagi Kasu,
    Description SusukiNiMochizuki Susuki Hikari,
    Description SusukiNiKari Susuki Tane,
    Description SusukiNoKasu1 Susuki Kasu,
    Description SusukiNoKasu2 Susuki Kasu,
    Description KikuNiSakazuki Kiku Tane,
    Description KikuNiAotan Kiku Aotan,
    Description KikuNoKasu1 Kiku Kasu,
    Description KikuNoKasu2 Kiku Kasu,
    Description MomijiNiShika Momiji Tane,
    Description MomijiNiAotan Momiji Aotan,
    Description MomijiNoKasu1 Momiji Kasu,
    Description MomijiNoKasu2 Momiji Kasu,
    Description YanagiNiMichikaze Yanagi Hikari,
    Description YanagiNiTsubame Yanagi Tane,
    Description YanagiNiTanzaku Yanagi Tanzaku,
    Description YanagiNoKasu Yanagi Kasu,
    Description KiriNiHouou Kiri Hikari,
    Description KiriNoKasu1 Kiri Kasu,
    Description KiriNoKasu2 Kiri Kasu,
    Description KiriNoKasu3 Kiri Kasu
  ]

deck :: [Card]
deck = map card cards

data Name
  = Tsuru
  | Uguisu
  | Mankai
  | Hototogisu
  | Yatsuhashi
  | Chou
  | Yamajishi
  | Mochizuki
  | Kari
  | Sakazuki
  | Shika
  | Michikaze
  | Tsubame
  | Houou
  deriving (Eq, Ord, Show)

fromName :: Name -> Card
fromName Tsuru = MatsuNiTsuru
fromName Uguisu = UmeNiUguisu
fromName Mankai = SakuraNiManmaku
fromName Hototogisu = FujiNiJichou
fromName Yatsuhashi = KakitsubataNiYatsuhashi
fromName Chou = BotanNiChou
fromName Yamajishi = HagiNiYamajishi
fromName Mochizuki = SusukiNiMochizuki
fromName Kari = SusukiNiKari
fromName Sakazuki = KikuNiSakazuki
fromName Shika = MomijiNiShika
fromName Michikaze = YanagiNiMichikaze
fromName Tsubame = YanagiNiTsubame
fromName Houou = KiriNiHouou

hasName :: Name -> Card -> Bool
hasName n c = fromName n == c

withRank :: Rank -> [Card] -> [Card]
withRank r = with ((== r) . rank)

withSuit :: Suit -> [Card] -> [Card]
withSuit s = with ((== s) . suit)

with :: (Description -> Bool) -> [Card] -> [Card]
with cond = map card . filter cond . descriptions

descriptions :: [Card] -> [Description]
descriptions cs = filter ((`elem` cs) . card) cards
