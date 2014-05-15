import Components.Card
import Components.Deck
import Components.Player
import Data.Char (toUpper)
import Data.List (intercalate)
import System.Random (getStdGen)

data Turn = Turn {getPlayer :: Player,
                      getAI :: Player,
                    getDeck :: Deck,
                 getDiscard :: Card} deriving Show

instance Eq Turn where
    (==) a b = getDiscard a == getDiscard b

extract :: Maybe a -> a
extract (Just a) = a

findPlayable :: Card -> Player -> [Int]
findPlayable (Card face suit) (Player hand) = foldr foldFunction [] processed
    where processed = zipWith (\x (a, b) -> (x, a, b)) [0..length hand - 1] 
                        . map (\(Card f' s') -> (f', s')) $ hand
          foldFunction (i, f', s') acc = if face == f' 
                                            || toUpper suit == toUpper s'
                                            || f' == 8 then
                                            i:acc else acc

initialize :: Deck -> Turn
--takes a shuffled deck and return 2 players that both have 8 cards along with the new deck
initialize shuffled = Turn (Player hand) (Player hand') deck'' $ extract discard
    where (hand, deck)   = drawNum 8 shuffled
          (hand', deck') = drawNum 8 deck
          (discard, deck'') = draw deck

advance :: Turn -> IO Turn
-- Player prompt and iput
advance t = do putStrLn $ "Computer is holding " ++ (show . length) aiHand 
                        ++ " cards"
               putStrLn $ "Discard pile : " ++ show discard
               putStrLn $ "Your hand: " ++ show player
               if playables == [] then drawOrPass else do
                   putStrLn $ "Which card do you want to discard (" ++ choice ++ ")?"
                   return t
--here!
    where
        player@(Player playerHand) = getPlayer t
        ai@(Player aiHand)         = getAI t
        deck                       = getDeck t
        discard                    = getDiscard t
        playables                  = findPlayable discard player
        choice                     = intercalate ", " 
                                     . map (\x -> show $ playerHand !! x)
                                     $ playables
        drawOrPass                 = do putStrLn "You don't have anything to play, so you drew"
                                        if drewCard == Nothing then 
                                            putStrLn "The deck is empty, so you passed"
                                            >> return t
                                        else 
                                            advance $ Turn (Player $ (extract drewCard):playerHand) ai deck' discard
            where (drewCard, deck') = draw deck

aiAdvance :: Turn -> IO Turn
aiAdvance t = return t

game :: Turn -> IO Turn
-- The game loop
game t = do
            playerTurn <- advance t
            aiTurn     <- aiAdvance playerTurn
            if playerTurn == aiTurn then
                putStrLn "The game ended in a draw!"
                >> return t
            else
                game aiTurn

--findPlayable (Card 4 'a') (Player [Card 6 'a',Card 4 'B',Card 8 'a', Card 0 'c'])
main = do
        putStrLn "Welcome to Crazy Eights!"
        putStrLn "Press enter to start"
        getLine
        gen <- getStdGen 
        next <- advance . initialize . (shuffle newDeck) $ gen
        return next