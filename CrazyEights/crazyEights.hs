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

isPlayable :: Turn -> Card -> Bool  -- not public facing
isPlayable t c = targetFace == actualFace ||
                 targetSuit == actualSuit || actualFace == 8
    where
        discard = getDiscard t
        targetFace = getFace discard
        targetSuit = getSuit discard
        actualFace = getFace c
        actualSuit = getSuit c

aiWon :: Turn -> Bool
aiWon t =

game :: Turn -> IO ()
game currentTurn
    | aiWon currentTurn = putStrln "\nThe computer won!\n"
    | playerWon currentTurn = putStrln "\nYou win!\n"
    | otherwise = do
        nextTurn <- advance' currentTurn
        if nextTurn == currentTurn then -- current turn passed
            nextNextTurn <- advance' nextTurn
            if nextNextTurn == currentTurn then
                putStrln "\nThe game ended in a draw!\n"
            else
                game nextNextTurn
        else
            game nextTurn


advance' :: Turn -> IO Turn
advance' t = do
    printStatus
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


advance :: Turn -> Either (Turn, Int) ([Card], Int -> Maybe Turn)