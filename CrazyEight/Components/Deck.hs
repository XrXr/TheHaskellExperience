module Components.Deck(newDeck) where

import System.Random
import Components.Card

newtype Deck = Deck [Card] deriving Show

newDeck :: Deck
newDeck = Deck $ map (\x -> Card x 'H') [1..13]
              ++ map (\x -> Card x 'D') [1..13]
              ++ map (\x -> Card x 'S') [1..13]
              ++ map (\x -> Card x 'C') [1..13]

draw :: Deck -> (Card, Deck)
draw (Deck (x:xs)) = (x, Deck xs)

shuffleDeck :: RandomGen gen => Deck -> Deck -> gen -> Deck
-- shuffle a Deck in O(n)
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