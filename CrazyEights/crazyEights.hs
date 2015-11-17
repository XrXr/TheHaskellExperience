import Components.Card
import Components.Deck
import Components.Player
import Data.Char (toUpper)
import Data.List (intercalate)
import System.Random (getStdGen, newStdGen, randomR)
import Control.Monad (when)
import Control.Concurrent (threadDelay)

data Turn = Turn {getPlayer :: Player,
                      getAI :: Player,
                    getDeck :: Deck,
                 getDiscard :: Card,
               getForceSuit :: Maybe Char,
               playerActive :: Bool} deriving Show

-- A turn, a list of possible discards and a function that returns the next
-- turn when passed a valid index that corresponds to a discard.
type SelectionInfo = (Turn, [Card], Int -> Maybe Turn)
-- The new turn after drawing and number of cards drew
type DrawInfo = (Turn, Int)

instance Eq Turn where
    (==) a b = getDiscard a == getDiscard b

isPlayable :: Card -> Card -> Bool
isPlayable expected actual = targetFace == actualFace ||
                             targetSuit == actualSuit || actualFace == 8
    where
        targetFace = getFace expected
        targetSuit = getSuit expected
        actualFace = getFace actual
        actualSuit = getSuit actual

aiWon :: Turn -> Bool
aiWon = finished . getAI

playerWon :: Turn -> Bool
playerWon = finished . getPlayer

activePlayerHand :: Turn -> [Card]
activePlayerHand t
    | playerActive t = playerHand
    | otherwise = aiHand
    where
        (Player playerHand) = getPlayer t
        (Player aiHand) = getAI t

game :: Turn -> IO ()
game currentTurn
    | aiWon currentTurn = putStrLn "\nThe computer won!\n"
    | playerWon currentTurn = putStrLn "\nYou win!\n"
    | otherwise = do
        nextTurn <- advance currentTurn
        if nextTurn == currentTurn then do -- current turn passed
            nextNextTurn <- advance nextTurn
            if nextNextTurn == currentTurn then
                putStrLn "\nThe game ended in a draw!\n"
            else
                game nextNextTurn
        else
            game nextTurn

pass :: Turn -> Turn
pass t = t{ playerActive = (not . playerActive) t}

turnSubject :: Turn -> String
turnSubject t = if playerActive t then "You" else "The computer"

showDrawAdvance :: DrawInfo -> String
showDrawAdvance (t, numDrew) =
    intercalate " " [turnSubject t, base, extra]
    where
        base = "drew " ++ (show numDrew) ++ " cards"
        extra = if (empty . getDeck) t then "and emptied the deck" else ""

promptAndMakeMove :: SelectionInfo -> IO Turn
promptAndMakeMove (oldTurn, playables, picker)
    | playerActive oldTurn = return oldTurn
    | otherwise = return . picker $ 0  -- amazing AI

-- Advance a turn. The turn in the result should have a different active player
advance :: Turn -> IO Turn
advance t = do
    printStatus t
    case findPlayables t of
        (Left drawAdvanceInfo) -> do
            putStrLn . showDrawAdvance $ drawAdvanceInfo
            if deckEmpty pendingNewTurn then
                return . pass $ pendingNewTurn
            else do
                putStrLn (turnSubject pendingNewTurn) ++ " was forced to pass :("
                advance . first $ drawAdvanceInfo
        (Right selectionInfo) -> do
            selection <- promptAndMakeMove selectionInfo
            printSelection selection
            return . first $ selection

-- Get a list of playables, or else draw until one is found
findPlayables :: Turn -> Either DrawInfo SelectionInfo
findPlayables t =
    if null playables then
        Left $ drawTillPlayable t
    else
        Right (t, playables, cardPicker)
    where
        activeHand = activePlayerHand t
        discard = getDiscard t
        forceSuit = getForceSuit t
        playables = filter (isPlayable discard) playerHand
        cardPicker i = if outOfRange then Nothing else Just (playables !! i)
            where outOfRange = i < 0 || i >= (lenth playables)
