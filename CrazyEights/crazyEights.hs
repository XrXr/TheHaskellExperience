import Components.Card
import Components.Deck
import Components.Player
import Data.Char (toUpper)
import Data.List (intercalate, splitAt)
import System.Random (getStdGen, newStdGen, randomR)
import Control.Monad (when)
import Data.Maybe (fromJust)
import Control.Concurrent (threadDelay)

data Turn = Turn {   player :: Player,
                         ai :: Player,
                       deck :: Deck,
                 getDiscard :: Card,
               getForceSuit :: Maybe Suit,
               playerActive :: Bool} deriving Show

-- A turn, a list of possible discards and a function that returns the next
-- turn when passed a valid index that corresponds to a discard.
type SelectionInfo = (Turn, [Card], Int -> Maybe Turn)
-- The new turn after drawing and number of cards drew
type DrawInfo = (Turn, Int)

instance Eq Turn where
    (==) a b = getDiscard a == getDiscard b

isPlayable :: Maybe Suit -> Card -> Card -> Bool
isPlayable maybeForceSuit expected actual =
    if actualFace == 8 then True else
        case maybeForceSuit of
            Nothing -> targetFace == actualFace ||
                       targetSuit == actualSuit
            (Just forcedSuit) -> targetFace == actualFace &&
                actualSuit == forcedSuit
    where
        targetFace = getFace expected
        targetSuit = getSuit expected
        actualFace = getFace actual
        actualSuit = getSuit actual

isCrazy :: Card -> Bool
isCrazy c = getFace c == 8

aiWon :: Turn -> Bool
aiWon = finished . ai

playerWon :: Turn -> Bool
playerWon = finished . player

activePlayerHand :: Turn -> [Card]
activePlayerHand t
    | playerActive t = playerHand
    | otherwise = aiHand
    where
        (Player playerHand) = player t
        (Player aiHand) = ai t

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
pass t = t{playerActive = (not . playerActive) t}

turnSubject :: Turn -> String
turnSubject t = if playerActive t then "You" else "The computer"

deckEmpty :: Turn -> Bool
deckEmpty = empty . deck

showDrawAdvance :: DrawInfo -> String
showDrawAdvance (t, numDrew) =
    intercalate " " [turnSubject t, base, extra]
    where
        base = "drew " ++ (show numDrew) ++ " cards"
        extra = if deckEmpty t then "and emptied the deck" else ""

promptAndMakeMove :: Turn -> IO Turn
promptAndMakeMove oldTurn
    | playerActive oldTurn = return oldTurn   -- TODO
    | otherwise = do
        if isCrazy firstPlayable then do
            gen <- newStdGen
            let suit = allSuits !! fst (randomR (0, 3) gen)
            putStrLn $ "Computer chose the suit to be " ++ show suit
            return oldTurn {
                getDiscard = firstPlayable,
                getForceSuit = Just suit,
                ai = Player newAIHand
            }
        else
            return oldTurn {
                getDiscard = firstPlayable,
                ai = Player newAIHand
            }
    where
        playables = findPlayables oldTurn
        firstPlayable = head playables
        newAIHand = tail playables

printStatus :: Turn -> IO ()
printStatus t = return ()  -- TODO

-- Advance a turn. The turn in the result should have a different active player
advance :: Turn -> IO Turn
advance t = do
    printStatus t
    if null playables then do
        let drawInfo@(turnAfterDraw, _) = drawTillPlayable t in do
            putStrLn . showDrawAdvance $ drawInfo
            if deckEmpty turnAfterDraw then do
                putStrLn $ (turnSubject t) ++ " was forced to pass :("
                return . pass $ turnAfterDraw
            else do
                advance turnAfterDraw
    else
        promptAndMakeMove t
    where
        playables = findPlayables t


addCardToActivePlayer :: Card -> Turn -> (Player, Player)
addCardToActivePlayer c t
    | playerActive t = (addCard c (player t), ai t)
    | otherwise = (player t, addCard c (ai t))

drawTillPlayable :: Turn -> DrawInfo
drawTillPlayable t = foldl folder (t, 0) [1..deckSize]
    where
        folder drawInfo@(t', numDrew) _ = case playables of
            [] -> case draw oldDeck of
                (Nothing, _) -> drawInfo
                (Just cardDrew, newDeck) ->
                    let (newPlayer, newAi) = addCardToActivePlayer cardDrew t'
                    in (t'{
                            deck = newDeck,
                            player = newPlayer,
                            ai = newAi
                        }, numDrew + 1)
            _ -> drawInfo
            where
                playables = findPlayables t'
                oldDeck = deck t'



findPlayables :: Turn -> [Card]
findPlayables t = filter (isPlayable forcedSuit discard) activeHand
    where
        activeHand = activePlayerHand t
        discard = getDiscard t
        forcedSuit = getForceSuit t

-- !! incorrect. Do not use.
-- Get a list of playables, or else draw until one is found
-- The force suit is not respected in the picker
possiblePlays :: Turn -> Either DrawInfo SelectionInfo
possiblePlays t =
    if null playables then
        Left $ drawTillPlayable t
    else
        Right (t, playables, cardPicker)
    where
        activeHand = activePlayerHand t
        discard = getDiscard t
        playables = findPlayables t
        cardPicker i = if outOfRange then Nothing else
            Just . pass $ if playerActive t then
                    t{
                        getDiscard = newDiscard,
                        player = discardAt i (player t)
                    }
                else
                    t{
                        getDiscard = newDiscard,
                        ai = discardAt i (ai t)
                    }
            where
                outOfRange = i < 0 || i >= (length playables)
                newDiscard = (playables !! i)

main :: IO ()
main = do
    putStrLn "Work in progress. Stay tuned!"
