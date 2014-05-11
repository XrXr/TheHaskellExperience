import Data.List
import Data.Maybe
import Numeric (showFFloat)


fib0 :: Int -> Int
fib0 n
    | n <= 0    = error "n must be larger than 0"
    | otherwise = n ^ 2

fib3 :: Int -> Integer
fib3 n = squence !! n
    where squence = 1:1:1:zipWith3 (\a b c -> a+b+c) squence (tail squence) (tail $ tail squence)

largest_two :: (Num a, Ord a) => [a] -> a
largest_two l = (last sorted) + (last . init $ sorted)
    where sorted = sort l

smallest_half :: (Num a, Ord a) => [a] -> a
smallest_half l = sum $ take (length sorted `div` 2) sorted
    where sorted = sort l

median :: (Num a, Ord a, Fractional a) => [a] -> a
median l
    | isEven     = (sorted !! middle + sorted !! (middle - 1)) / 2
    | otherwise  = sorted !! middle
    where sorted = sort l
          middle = length sorted `div` 2
          isEven = length sorted `mod` 2 == 0 

majority :: (Num a, Ord a) => [a] -> Maybe a
-- In an even length sorted list where a majority exist, its either
-- [a,a,a,a,b,b] or [b,b,a,a,a,a] both has the majority element at 
-- length `div` 2
-- in an odd length list
-- [a,a,a,b,b] or [b,b,a,a,a] this still hold
majority l
    | null l     = Nothing
    | isEven       = if same (take (middle + 1) sorted)
                   || same (drop (middle - 1) sorted)
                   then Just $ sorted !! middle else Nothing
    | otherwise  = if same (take (middle + 1) sorted)
                   || same (drop middle sorted)
                   then Just $ sorted !! middle else Nothing
    where sorted = sort l
          isEven = length sorted `mod` 2 == 0 
          middle = length sorted `div` 2
          same l@(x:xs) = replicate (length l) x == l

majority' :: Eq a => [a] -> Maybe a
-- Polymorphic version
majority' [] = Nothing
majority' l  = foldl' (\acc x -> if isMajority (find x) then Just x else acc) Nothing l 
    where middle       = length l `div` 2
          find e       = elemIndices e l
          isMajority a = length a >= middle + 1

canadian_change :: (Integral a, Show a) => a -> IO()
-- I still haven't learned about IO yet and monads
canadian_change n = putStr result
    where twoDecimal a      = showFFloat (Just 2) a ""
          rounded           = if n `mod` 5 < 3 then n - n `mod` 5 else n - n `mod` 5 + 5
          change acc x      = snd acc `div` x
          remaind acc x     = snd acc `mod` x
          faceValues        = [100, 50, 20, 10, 5, 2, 1, 0.25, 0.1, 0.05]
          inCents           = map (floor . (*100)) faceValues
          raw               = tail $ scanl (\acc x -> (change acc x, remaind acc x)) (0, rounded) inCents
          processed         = zip faceValues $ map fst raw
          printOut acc x    = if snd x /= 0 then acc ++ show (snd x) ++ " x $" ++ twoDecimal (fst x) ++ "\n" else acc
          result            = "$" ++ twoDecimal (fromIntegral n / 100) 
                              ++ " get rounded to $" ++ twoDecimal (fromIntegral rounded / 100) ++ "\n"
                              ++ foldl' printOut "" processed


main = putStrLn "Hello World"