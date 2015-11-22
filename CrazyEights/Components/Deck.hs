module Components.Deck(newDeck, draw, drawNum, shuffle, Deck, emptyDeck, empty, deckSize) where

import System.Random (RandomGen, randomR)
import Components.Card

newtype Deck = Deck [Card]

instance Show Deck where
    show (Deck xs) = listCards xs

newDeck :: Deck
newDeck = Deck $ [Card face suit | face <- [1..13], suit <- allSuits]

deckSize :: Int
deckSize = 13 * 4

emptyDeck :: Deck
emptyDeck = Deck []

empty :: Deck -> Bool
empty (Deck d) = null d

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
