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
            getPlayerActive :: Bool} deriving Show

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
    | getPlayerActive t = playerHand
    | otherwise = aiHand
    where
        (Player playerHand) = getPlayer t
        (Player aiHand) = getAI t

game :: Turn -> IO ()
game currentTurn
    | aiWon currentTurn = putStrln "\nThe computer won!\n"
    | playerWon currentTurn = putStrln "\nYou win!\n"
    | otherwise = do
        nextTurn <- advance' currentTurn
        if nextTurn == currentTurn then do -- current turn passed
            nextNextTurn <- advance' nextTurn
            if nextNextTurn == currentTurn then
                putStrln "\nThe game ended in a draw!\n"
            else
                game nextNextTurn
        else
            game nextTurn


advance' :: Turn -> IO Turn
advance' t = do
    printStatus t
    case of pendingNewTurn
        (Left drawAdvanceInfo) -> do
            printDrawAdvance drawAdvanceInfo
            returnFirst drawAdvanceInfo
        (Right selectionConstrain) -> do
            printOptions selectionConstrain
            selection <- selectCard selectionConstrain
            printSelection selection
            returnFirst selection
    where
        pendingNewTurn = advance t
        returnFirst = return . first

-- Advance the current turn. Left is returned when there are no possible
-- discards. The Int is the number of cards drawn until a card is discarded.
-- Right happens when there is at least one card to discard. It contains a list
-- of possible discards and a function that returns the next turn when passed
-- a valid index that corresponds to a discard.
advance :: Turn -> Either (Turn, Int) ([Card], Int -> Maybe Turn)
advance t =
    if null playables then
        Left $ drawAdvance t
    else
        Right (playables, cardPicker)
    where
        activeHand = activePlayerHand t
        discard = getDiscard t
        forceSuit = getForceSuit t
        playables = filter (isPlayable discard) playerHand
        cardPicker i = if outOfRange then Nothing else Just (playables !! i)
            where outOfRange = i < 0 || i >= (lenth playables)
