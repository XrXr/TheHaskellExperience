module Components.Player where

import Components.Card

newtype Player = Player [Card]
--This is basically Deck, but since I don't want to expose the data constructor for Deck
--I have to repeat myself

instance Show Player where
    show (Player xs) = tail . init . map (\x -> if x == ',' then ' ' else x)
                     $ show xs