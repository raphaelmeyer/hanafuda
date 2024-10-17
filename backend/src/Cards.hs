module Cards where

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

data Points = Hikari | Tane | Tan | Kasu deriving (Eq, Ord, Show)

data Name
  = Tsuru
  | Mankai
  | Mochiduki
  | Michikaze
  | Houou
  | Uguisu
  | Jichou
  | Yatsuhashi
  | Chou
  | Yamajishi
  | Kari
  | Sakazuki
  | Shika
  | Tsubame
  | Akatan
  | Aotan
  | Tanzaku
  | One
  | Two
  | Three
  deriving (Eq, Ord, Show)

data Card = Card {suit :: Suit, points :: Points, name :: Name}
  deriving (Eq, Ord, Show)

deck :: [Card]
deck =
  [ Card Matsu Hikari Tsuru,
    Card Matsu Tan Akatan,
    Card Matsu Kasu One,
    Card Matsu Kasu Two,
    Card Ume Tane Uguisu,
    Card Ume Tan Akatan,
    Card Ume Kasu One,
    Card Ume Kasu Two,
    Card Sakura Hikari Mankai,
    Card Sakura Tan Akatan,
    Card Sakura Kasu One,
    Card Sakura Kasu Two,
    Card Fuji Tane Jichou,
    Card Fuji Tan Tanzaku,
    Card Fuji Kasu One,
    Card Fuji Kasu Two,
    Card Kakitsubata Tane Yatsuhashi,
    Card Kakitsubata Tan Tanzaku,
    Card Kakitsubata Kasu One,
    Card Kakitsubata Kasu Two,
    Card Botan Tane Chou,
    Card Botan Tan Aotan,
    Card Botan Kasu One,
    Card Botan Kasu Two,
    Card Hagi Tane Yamajishi,
    Card Hagi Tan Tanzaku,
    Card Hagi Kasu One,
    Card Hagi Kasu Two,
    Card Susuki Hikari Mochiduki,
    Card Susuki Tane Kari,
    Card Susuki Kasu One,
    Card Susuki Kasu Two,
    Card Kiku Tane Sakazuki,
    Card Kiku Tan Aotan,
    Card Kiku Kasu One,
    Card Kiku Kasu Two,
    Card Momiji Tane Shika,
    Card Momiji Tan Aotan,
    Card Momiji Kasu One,
    Card Momiji Kasu Two,
    Card Yanagi Hikari Michikaze,
    Card Yanagi Tane Tsubame,
    Card Yanagi Tan Tanzaku,
    Card Yanagi Kasu One,
    Card Kiri Hikari Houou,
    Card Kiri Kasu One,
    Card Kiri Kasu Two,
    Card Kiri Kasu Three
  ]
