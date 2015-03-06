module CrazyEights where

import Components.Card
import Components.Deck
import Components.Player
import Data.Char (toUpper)
import Data.List (intercalate)
import System.Random (getStdGen, newStdGen, randomR)
import Control.Monad (when)
import Control.Concurrent (threadDelay)
import Data.Maybe (isNothing, fromJust)

data Turn = Turn { activePlayer :: Player,
                    otherPlayer :: Player,
                           deck :: Deck,
                         discard :: Card,
                      forceSuit :: Maybe Suit,
                       aiActive :: Bool} deriving Show

type SuitPendingTurn = Suit -> Turn

instance Eq Turn where
    (==) a b = discard a == discard b

removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt i l  = begin ++ drop 1 end
    where (begin, end) = splitAt i l

pause :: IO ()
pause = threadDelay 1000000

-- |Match a card aginst a hand or a face. Return a list of indicies in the
--  hand that are playable cards
findPlayable :: Card -> Player -> Maybe Suit -> [Int]
findPlayable (Card face suit) (Player hand) required_suit
    = map fst . filter (playable required_suit) $ processed
    where processed              = zip [0..length hand - 1]
                                   . map (\(Card f' s') -> (f', s')) $ hand
          playable Nothing
                   (_, (f', s')) = face == f' || suit == s' || f' == 8
          playable (Just required)
                   (_, (f', s')) = required == s' || f' == 8

-- |Draw cards until the current player can play a card, then play that card.
--  If there is no cards left in the deck, then current player will skip the
--  turn. Return a tuple with the number of cards drawn, and the new Turn, or
--  a tuple with the number of cards drawn and a SuitPendingTurn
drawOrPass :: Turn -> (Int, Either Turn SuitPendingTurn)
drawOrPass turn = drawCards 0 (activePlayer turn) (deck turn)
    where
      drawCards n p d
          | (not . null) playables = if isEight cardToPlay then
                (n, Right pending) else (n, Left playedTurn)
          | isNothing cardDrew     = (n, Left passedTurn)
          | otherwise              = drawCards (n + 1) newPlayer d'

          where
            (cardDrew, d') = draw d
            playables      = findPlayable (discard turn) p (forceSuit turn)
            cardToPlay     = getHand p !! head playables
            pending s      = playedTurn {forceSuit = Just s}
            newPlayer      = addCard (fromJust cardDrew) p
            playedTurn     = turn { deck = d,
                                    discard = cardToPlay,
                                    otherPlayer = p,
                                    activePlayer = otherPlayer turn,
                                    aiActive = not . aiActive $ turn,
                                    forceSuit = Nothing}
            passedTurn     = turn { deck = d,
                                    otherPlayer = p,
                                    activePlayer = otherPlayer turn,
                                    aiActive = not . aiActive $ turn}
