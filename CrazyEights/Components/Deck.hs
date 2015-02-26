module Components.Deck(newDeck, draw, drawNum, shuffle, Deck, emptyDeck) where

import System.Random (RandomGen, randomR)
import Components.Card

newtype Deck = Deck [Card]

instance Show Deck where
    show (Deck xs) = tail . init . map (\x -> if x == ',' then ' ' else x)
                     $ show xs

newDeck :: Deck
newDeck = Deck $ map (\x -> Card x H) [1..13]
              ++ map (\x -> Card x D) [1..13]
              ++ map (\x -> Card x S) [1..13]
              ++ map (\x -> Card x C) [1..13]

emptyDeck :: Deck
emptyDeck = Deck []

draw :: Deck -> (Maybe Card, Deck)
draw (Deck [])     = (Nothing, Deck [])
draw (Deck (x:xs)) = (Just x, Deck xs)

drawNum :: Int -> Deck -> ([Card], Deck)
drawNum n (Deck l) = (take n l, Deck $ drop n l)

shuffleDeck :: RandomGen gen => Deck -> Deck -> gen -> Deck
-- shuffle a Deck in O(n^2) (!! is O(n))
-- never thought I would have to write this myself
shuffleDeck shuffled (Deck []) _ = shuffled
shuffleDeck (Deck shuffled) (Deck original) gen 
            = shuffleDeck (Deck shuffled') (Deck original') gen'
            where (elemIndex, gen') = randomR (0, length original - 1) gen
                  shuffled' = original !! elemIndex : shuffled
                  original' = begin ++ tail end 
                            where (begin, end) = splitAt elemIndex original 

shuffle :: RandomGen gen => Deck -> gen -> Deck
shuffle deck = shuffleDeck (Deck []) deck
