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
               getForceSuit :: Maybe Char} deriving Show

instance Eq Turn where
    (==) a b = getDiscard a == getDiscard b

extract :: Maybe a -> a
extract (Just a) = a

removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt i l  = begin ++ drop 1 end
    where (begin, end) = splitAt i l

pause :: IO()
pause = threadDelay 1000000

-- |Match a card aginst a hand or a face. Return a list of indicies in the
--  hand that are playable cards
findPlayable :: Card -> Player -> Maybe Suit -> [Int]
findPlayable (Card face suit) (Player hand) required_suit
    = foldr foldFunction [] processed
    where processed = zipWith (\i (f', s') -> (i, f', s')) [0..length hand - 1]
                        . map (\(Card f' s') -> (f', s')) $ hand
          foldFunction (i, f', s') acc = case required_suit of
                  Nothing               -> if face == f' || suit == s'
                                           || f' == 8 then
                                              i:acc else acc
                  (Just required_suit') -> if required_suit' == s'
                                           || f' == 8 then
                                              i:acc else acc

-- |Takes a shuffled deck and return 2 players that 
--  both have 8 cards along with the new deck.
initialize :: Deck -> Turn
initialize shuffled = Turn (Player hand) (Player hand') deck''
    (extract discard) Nothing
    where (hand, deck)   = drawNum 8 shuffled
          (hand', deck') = drawNum 8 deck
          (discard, deck'') = draw deck'

advance :: Turn -> IO Turn
-- Player prompt and iput
advance t = do putStrLn $ "Computer is holding " ++ (show . length) aiHand
                        ++ " cards"
               putStrLn $ "Discard pile : " ++ show discard
               putStrLn $ "Your hand: " ++ show player
               when (suit /= Nothing) (putStrLn $ "You must match the "
                                       ++ (show . extract) suit ++ " suit")
               if playables == [] then do
                    putStrLn "\nYou don't have anything to play, so you drew"
                    pause
                    drawOrPass 0 player deck
                else do
                    putStrLn $ "Which card do you want to discard (" 
                        ++ choice ++ ")?"
                    selection <- getLine
                    validate (reads selection :: [(Int, String)])
    where
        player@(Player playerHand) 
                           = getPlayer t
        ai@(Player aiHand) = getAI t
        deck               = getDeck t
        suit               = getForceSuit t
        discard            = getDiscard t
        playables          = findPlayable discard player suit
        choice             = intercalate ", "
                             . zipWith (++) (map (\x -> show x ++ "=") [1..])
                             . map (\x -> show $ playerHand !! x)
                             $ playables
        drawOrPass n p' d' = if length playables >= 1 then do
                                putStrLn $ "You drew " ++ show n 
                                    ++ " cards then played "
                                    ++ show cardToPlay
                                if getFace cardToPlay == 8 then do
                                    s' <- inputSuit
                                    return 
                                        $ Turn played ai d' cardToPlay
                                        $ Just s'
                                else
                                    return
                                        $ Turn played ai d' cardToPlay
                                        Nothing
                             else
                                if drewCard == Nothing then do 
                                    putStrLn $ "You drew " ++ show n
                                        ++ " cards and emptied the deck\
                                        \, so you passed"
                                    return $ Turn p' ai d' discard suit
                                else
                                    drawOrPass (n + 1) p'' deck'
            where playables  = findPlayable discard p' suit
                  -- these only get evaluated when playables is not empty
                  cardToPlay = getHand p' !! head playables
                  (drewCard, deck') 
                             = draw d'
                  p''        = addCard (extract drewCard) p'
                  played     = Player 
                               $ removeAt (head playables) (getHand p')
        validate []        = putStrLn "Invalid input\n"
                             >> advance t
        validate [(a, _)]  = if a `elem` [1..length playables] then do
                                putStrLn $ "You played " ++ show d'
                                if getFace d' == 8 then do 
                                    suit' <- inputSuit
                                    return $ Turn p' ai deck d' $ Just suit'
                                else
                                    return $ Turn p' ai deck d' Nothing
                             else
                                validate []
            where choosen = playables !! (a - 1)
                  d' = playerHand !! choosen
                  p' = Player $ removeAt choosen playerHand
        inputSuit          = do putStrLn "You have played an eight, please \
                                \pick a suit the computer has to play"
                                putStrLn "(H, D, S, C)"
                                selection <- getLine
                                validate selection
            where validate (c:[]) = if toUpper c
                                        `elem` ['H', 'D', 'S', 'C'] then
                                        return $ toUpper c
                                    else
                                        validate "Fail"
                  validate _      = putStrLn "Invalid input\n"
                                     >> inputSuit

aiAdvance :: Turn -> IO Turn
aiAdvance t = do 
    if length playables >= 1 then do
        putStrLn $ "Computer played " ++ show cardToPlay
        if getFace cardToPlay == 8 then
            pickSuit played deck cardToPlay
        else
            return $ Turn player played deck cardToPlay Nothing
    else
        drawOrPass 0 ai deck
    where
        player             = getPlayer t
        ai@(Player aiHand) = getAI t
        deck               = getDeck t
        suit               = getForceSuit t
        discard            = getDiscard t
        playables          = findPlayable discard ai suit
        cardToPlay         = aiHand !! head playables
        played             = Player 
                                $ removeAt (head playables) aiHand
        pickSuit p d c     = do 
                                gen <- newStdGen
                                let suit = ['H', 'D', 'S', 'C']
                                     !! fst (randomR (0, 3) gen)
                                putStrLn $ "Computer chose to suit to be " 
                                           ++ [suit]
                                return $ Turn player p d c (Just suit)
        drawOrPass n p' d' = if length playables >= 1 then do
                                 putStrLn $ "Computer drew " ++ show n
                                    ++ " cards then played "
                                    ++ show cardToPlay
                                 if getFace cardToPlay == 8 then
                                    pickSuit played d' cardToPlay
                                 else
                                    return 
                                        $ Turn player played d' cardToPlay
                                    Nothing
                             else
                                if drewCard == Nothing then do
                                    putStrLn $ "Computer drew " ++ show n
                                        ++ " cards and emptied the deck"
                                    putStrLn "Computer passes"
                                    return 
                                        $ Turn player p' d' discard 
                                        suit
                                else
                                    drawOrPass (n + 1) 
                                        (addCard (extract drewCard) p') deck'
            where playables  = findPlayable discard p' suit
                  cardToPlay = getHand p' !! head playables
                  played     = Player 
                               $ removeAt (head playables) (getHand p')
                  (drewCard, deck') 
                             = draw d'

game :: Turn -> IO Turn
-- The game loop
game t = do
            --print t
            putStrLn ""
            playerTurn <- advance t
            --print playerTurn
            pause
            if playerWon playerTurn then
                putStrLn "\nYou win!\n"
                >> return t
            else do
                putStrLn ""
                aiTurn <- aiAdvance playerTurn
                --print aiTurn
                pause
                if aiWon aiTurn then
                    putStrLn "\nThe computer won!\n"
                    >> return t
                else
                    -- both players passed, game is a draw
                    if t == playerTurn && t == aiTurn then
                        putStrLn "\nThe game ended in a draw!\n"
                        >> return t
                    else
                        game aiTurn
    where
        playerWon = (\(Player hand) -> null hand) . getPlayer
        aiWon = (\(Player hand) -> null hand) . getAI
--findPlayable (Card 4 'a') 
--(Player [Card 6 'a',Card 4 'B',Card 8 'a', Card 0 'c'])
main = do
        putStrLn "Welcome to Crazy Eights!"
        putStrLn "Press enter to start"
        getLine
        gen <- getStdGen
        game . initialize . (shuffle newDeck) $ gen
        pause
        return ()
