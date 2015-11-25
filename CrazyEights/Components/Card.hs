module Components.Card where

import Data.Char

data Suit = C | D | S | H deriving(Show, Eq, Read)

allSuits :: [Suit]
allSuits = [C, D, S, H]

char :: Suit -> Char
char = head . show

data Card = Card {getFace :: Int, getSuit :: Suit} deriving(Eq)
instance Show Card where
    show (Card face suit)
        | face == 1  = combine 'A'
        | face == 11 = combine 'J'
        | face == 12 = combine 'Q'
        | face == 13 = combine 'K'
        | otherwise  = show face ++ show suit
        where
            combine a = [a, char suit]

listCards :: [Card] -> String
listCards [] = "<empty deck>"
listCards xs = tail . (foldr (\x acc -> ' ' : (show x) ++ acc) "") $ xs