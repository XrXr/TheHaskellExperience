module Components.Card where

import Data.Char

data Card = Card Int Char
instance Show Card where
    show (Card face suit)
        | face == 1  = 'A' : toUpper suit : []
        | face == 11 = 'J' : toUpper suit : []
        | face == 12 = 'Q' : toUpper suit : []
        | face == 13 = 'K' : toUpper suit : []
        | otherwise  = show face ++ [toUpper suit]