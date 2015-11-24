module Components.Player where

import Components.Card

newtype Player = Player [Card]

instance Show Player where
    show (Player xs) = listCards xs

addCard :: Card -> Player -> Player
addCard c (Player l) = Player $ c:l

finished :: Player -> Bool
finished (Player l) = null l

discardAt :: Int -> Player -> (Player, Card)
discardAt i (Player hand) = (Player newHand, discarded)
    where
        (newHand, discarded) = removeAt i hand

-- not very efficient, but cool
removeAt :: Int -> [a] -> ([a], a)
removeAt i l = (a ++ b, removed)
    where
        (a, removed:b) = splitAt i l