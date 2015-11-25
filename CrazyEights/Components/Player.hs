module Components.Player where

import Components.Card

newtype Player = Player [Card]

instance Show Player where
    show (Player xs) = listCards xs

addCard :: Card -> Player -> Player
addCard c (Player l) = Player $ c:l

finished :: Player -> Bool
finished (Player l) = null l

without :: Eq a => a -> [a] -> [a]
without elem list = if null after then before else before ++ tail after
    where
        (before, after) = break ((==) elem) list

discardCard :: Card -> Player -> Player
discardCard c (Player oldHand) = Player $ without c oldHand