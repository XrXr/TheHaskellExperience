module Components.Card(Card(..), Suit(..)) where

import Data.Char

data Card = Card {getFace :: Int, getSuit :: Suit}
instance Show Card where
    show (Card face suit)
        | face == 1  = 'A' : (head . show) suit : []
        | face == 11 = 'J' : (head . show) suit : []
        | face == 12 = 'Q' : (head . show) suit : []
        | face == 13 = 'K' : (head . show) suit : []
        | otherwise  = (head . show) face : show suit

data Suit = C | D | H | S deriving (Show, Eq)

instance Eq Card where
    (==) (Card a b) (Card c d) = a == c && b == d
