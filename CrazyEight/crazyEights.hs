import Components.Card
import Components.Deck
import Components.Player
import Data.Char (toUpper)

findPlayable :: Card -> Player -> [Int]
findPlayable (Card face suit) (Player hand) = foldr foldFunction [] processed
    where processed = zipWith (\x (a, b) -> (x, a, b)) [0..length hand - 1] 
                        . map (\(Card f' s') -> (f', s')) $ hand
          foldFunction (i, f', s') acc = if face == f' 
                                            || toUpper suit == toUpper s'
                                            || f' == 8 then
                                            i:acc else acc
          
--findPlayable (Card 4 'a') (Player [Card 6 'a',Card 4 'B',Card 8 'a', Card 0 'c'])
main = print $ newDeck