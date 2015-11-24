import Components.Card
import Components.Deck
import Components.Player
import Data.Char (toUpper)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import System.Random (newStdGen, randomR, getStdGen)
import Control.Concurrent (threadDelay)

data Turn = Turn {   player :: Player,
                         ai :: Player,
                       deck :: Deck,
                 getDiscard :: Card,
               getForceSuit :: Maybe Suit,
               playerActive :: Bool} deriving Show

-- The new turn after drawing and number of cards drew
type DrawInfo = (Turn, Int)

instance Eq Turn where
    (==) a b = getDiscard a == getDiscard b

pause :: IO ()
pause = threadDelay 1000

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

ansiGreen :: String -> String
ansiGreen s = "\ESC[32m" ++ s ++ "\ESC[m"

blankLine :: IO ()
blankLine = putStrLn ""

printPlayables :: [Card] -> String
printPlayables cards = intercalate " " zipped
    where
        zipped = zipWith zipper (map show cards) [1..length cards]
        zipper :: String -> Int -> String
        zipper s i = ansiGreen (show i ++ ".") ++ s

data Selection = Chose Int | DrawCard deriving(Show)

-- Take a number that is the lowest invalid selection
askForSelection :: Int -> IO Selection
askForSelection i = do
    putStrLn $ "Type <" ++ ansiGreen "number" ++
               "> to pick card, or \"draw\" to draw a card"
    input <- getLine
    if input == "draw" then
        return $ DrawCard
    else do
        let parseResult = reads input :: [(Int, String)]
            selection = fst . head $ parseResult
            fullConsumption = null . snd . head $ parseResult
        if null parseResult || not fullConsumption || outOfRange selection then
            askForSelection i
        else
            return $ Chose selection
    where
        outOfRange j = j < 0 || j >= i

askForSuit :: IO Suit
askForSuit = do
    input <- getLine
    let normalized = toUpper . head $ input
        parseResult = reads [normalized] :: [(Suit, String)]
    if length input /= 1 || null parseResult then do
        putStrLn "Invalid suit. Try again: (C, D, S, H)"
        askForSuit
    else
        return . fst . head $ parseResult

promptAndMakeMove :: Turn -> IO Turn
promptAndMakeMove oldTurn
    | playerActive oldTurn = do
        putStrLn "It's your turn"
        putStrLn "Cards you can play:"
        putStrLn . printPlayables $ playables
        blankLine
        selection <- askForSelection (length playables)
        case selection of
            DrawCard -> case draw . deck $ oldTurn of
                (Just cardDrew, deckAfterDraw) -> do
                    let (newPlayer, _) = addCardToActivePlayer cardDrew oldTurn
                    return oldTurn{
                        player = newPlayer,
                        deck = deckAfterDraw
                    }
                (Nothing, _) -> do
                    putStrLn "Cannot draw from an empty deck :("
                    promptAndMakeMove oldTurn
            (Chose idx) -> do
                let (afterPlay, played) = discardAt idx (player oldTurn)
                    turnWithPlayMade = pass oldTurn{
                        getDiscard = played,
                        player = afterPlay
                    }
                if isCrazy played then do
                    putStrLn "You have played an eight. Please choose a suit \
                        \(C, D, S, H)"
                    suit <- askForSuit
                    return turnWithPlayMade { getForceSuit = Just suit }
                else
                    return turnWithPlayMade
    | otherwise = do
        let newAIHand = tail playables
            firstPlayable = head playables
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

printStatus :: Turn -> IO ()
printStatus t = do
    putStrLn $ "The computer is holding " ++ aiSize ++ " cards"
    case getForceSuit t of
        (Just suit) -> do
            pause
            putStrLn $ subject ++ " must play a card from the " ++ show suit ++ " suit"
        _ -> return ()
    blankLine
    where
        (Player aiHand) = ai t
        subject = turnSubject t
        aiSize = show . length $ aiHand

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
    else do
        nextTurn <- promptAndMakeMove t
        return . pass $ nextTurn
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

dealStartingHand :: Deck -> Turn
dealStartingHand fullDeck = Turn {
    player = (Player playerHand),
    ai = (Player aiHand),
    deck = deck'',
    getDiscard = (fromJust discard),
    getForceSuit = Nothing,
    playerActive = True
}
    where (playerHand, deck)   = drawNum 8 fullDeck
          (aiHand, deck') = drawNum 8 deck
          (discard, deck'') = draw deck'

main :: IO ()
main = do
    putStrLn "Welcome to Crazy Eights!"
    putStrLn "Press enter to start"
    getLine
    gen <- getStdGen
    game . dealStartingHand . (shuffle newDeck) $ gen
    pause
