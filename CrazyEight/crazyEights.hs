import Components.Card
import Components.Deck
import Components.Player
import Data.Char (toUpper)
import Data.List (intercalate)
import System.Random (getStdGen)

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


-- |Match a card aginst a hand or a face
findPlayable :: Card -> Player -> Maybe Char -> [Int]
findPlayable (Card face suit) (Player hand) force
    = foldr foldFunction [] processed
    where processed = zipWith (\x (a, b) -> (x, a, b)) [0..length hand - 1] 
                        . map (\(Card f' s') -> (f', s')) $ hand
          foldFunction (i, f', s') acc 
              | force == Nothing = if face == f' 
                                   || toUpper suit == toUpper s'
                                   || f' == 8 then
                                       i:acc else acc
              | otherwise        = if toUpper (extract force) == toUpper s'
                                   || f' == 8 then
                                       i:acc else acc

-- |Takes a shuffled deck and return 2 players that 
-- both have 8 cards along with the new deck.
initialize :: Deck -> Turn
initialize shuffled = Turn (Player hand) (Player hand') deck''
    (extract discard) Nothing
    where (hand, deck)   = drawNum 8 shuffled
          (hand', deck') = drawNum 8 deck
          (discard, deck'') = draw deck

advance :: Turn -> IO Turn
-- Player prompt and iput
advance t = do putStrLn $ "Computer is holding " ++ (show . length) aiHand
                        ++ " cards"
               putStrLn $ "Discard pile : " ++ show discard
               putStrLn $ "Your hand: " ++ show player
               if playables == [] then do
                    putStrLn "You don't have anything to play, so you drew"
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
                                        $ Turn p' ai d' cardToPlay $ Just s'
                                else
                                    return $ Turn p' ai d' cardToPlay suit
                             else
                                if drewCard == Nothing then do 
                                    putStrLn $ "You drew " ++ show n
                                        ++ " cards and emptied the deck\
                                        \, so you passed"
                                    return $ Turn p' ai d' cardToPlay suit
                                else
                                    drawOrPass (n + 1) p'' deck'
            where playables  = findPlayable discard p' suit
                  -- this only gets evaluated when playables is not empty
                  cardToPlay = (\(Player ph) -> ph) p' !! head playables
                  (drewCard, deck') 
                             = draw d'
                  p''        = addCard (extract drewCard) p'
        validate []        = putStrLn "Invalid input"
                             >> putStrLn ""
                             >> advance t
        validate [(a, _)]  = if a `elem` [1..length playables] then
                                if getFace d' == 8 then do 
                                    suit' <- inputSuit
                                    return $ Turn p' ai deck d' $ Just suit'
                                else
                                    return $ Turn p' ai deck d' suit
                             else
                                validate []
            where choosen = playables !! (a - 1)
                  d' = playerHand !! choosen
                  p' = Player $ removeAt choosen playerHand
        inputSuit          = do putStrLn "You have played an eight, please \
                                \pick a suit the computer has to play"
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
aiAdvance t = return t

game :: Turn -> IO Turn
-- The game loop
game t = do
            playerTurn <- advance t
            if playerWon playerTurn then
                putStrLn "\nYou win!\n"
                >> return t
            else do
                aiTurn <- aiAdvance playerTurn
                if aiWon aiTurn then
                    putStrLn "\nThe computer won!\n"
                    >> return t
                else
                    if playerTurn == aiTurn then
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
        next <- advance . initialize . (shuffle newDeck) $ gen
        return next