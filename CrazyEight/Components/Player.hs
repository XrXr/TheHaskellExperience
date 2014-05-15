module Components.Player where

import Components.Card

newtype Player = Player [Card]

instance Show Player where
    show (Player xs) = tail . init . map (\x -> if x == ',' then ' ' else x)
                     $ show xs