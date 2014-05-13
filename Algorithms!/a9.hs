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
    | isEven     = if same (take (middle + 1) sorted)
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
-- I still haven't learned about IO and monads yet
canadian_change n = putStr result
    where twoDecimal a   = showFFloat (Just 2) a ""
          rounded        = if n `mod` 5 < 3 then n - n `mod` 5 else n - n `mod` 5 + 5
          change acc x   = snd acc `div` x
          remaind acc x  = snd acc `mod` x
          faceValues     = [100, 50, 20, 10, 5, 2, 1, 0.25, 0.1, 0.05]
          inCents        = map (floor . (*100)) faceValues
          raw            = tail $ scanl (\acc x -> (change acc x, remaind acc x)) (0, rounded) inCents
          processed      = zip faceValues $ map fst raw
          printOut acc x = if snd x /= 0 then acc ++ show (snd x) ++ " x $" ++ twoDecimal (fst x) ++ "\n" else acc
          result         = "$" ++ twoDecimal (fromIntegral n / 100) 
                           ++ " get rounded to $" ++ twoDecimal (fromIntegral rounded / 100) ++ "\n"
                           ++ foldl' printOut "" processed

triple_sum :: (Eq a, Num a) => [a] -> a -> Maybe (Int, Int, Int)
-- This version assumes the elements in the list are unique
-- triple_sum [1, 5, 5, 8, 2, 6, 55, 90] 100 returns Nothing
triple_sum l x 
    | canidates == []     = Nothing 
    | isGoodResult result = Just result
    | otherwise           = Nothing
    where addTo a                = [(e + a, e, a) | e <- l, a /= e]
          twoSums                = foldr (\e acc -> addTo e ++ acc) [] l  -- list of 2-sums and their elements
          canidates              = filter (\(a, _, _) -> x - a `elem` l) twoSums
          find a                 = (\(Just e) -> e) $ elemIndex a l
          findResult (a, b, c)   = (find b, find c, find (x - (b + c)))
          result                 = findResult (head canidates)
          isGoodResult (a, b, c) = a /= b && a /= c && b /= c

triple_sum' :: (Eq a, Num a) => [a] -> a -> Maybe (Int, Int, Int)
-- This version works on any list
-- triple_sum' [1, 5, 5, 8, 2, 6, 55, 90] 100 returns Just (1,2,7)
triple_sum' l x
    | canidates == [] = Nothing
    | result == []    = Nothing
    | otherwise       = (\(a, b, [c]) -> Just (a, b, c)) $ head result
    where addTo a                = [(e + a, e, a) | e <- delete a l]
          twoSums                = foldr (\e acc -> addTo e ++ acc) [] l  -- list of 2-sums and their elements
          canidates              = filter (\(a, _, _) -> x - a `elem` l) twoSums -- x - sum of 2 is in the list
          find a                 = (\(Just e) -> e) $ elemIndex a l
          findSecond first a     = head $ filter (\x -> x /= first) $ elemIndices a l
          findThird first second = filter (\x -> x /= first && x /= second) $
                                        elemIndices (x - (l !! first + l !! second)) l
          makeTirple (_, b, c)   = (first, second, third)
                                    where first  = find b
                                          second = findSecond first c
                                          third  = findThird first second
          possibleResult         = map makeTirple canidates
          result                 = filter (\(_, _, c) -> c /= []) possibleResult

bentley :: (Ord a, Num a) => [a] -> Maybe (Int, Int)
-- This isn't the real bentley algorithm
bentley [] = Nothing
bentley l = (\(s, e, _) -> Just (s, e)) maxRange
    where range     = [0..length l - 1]
          ranges    = concat $ map makeRange range
          makeRange element = foldr (\x acc -> (element, x) : acc) [] $ drop element range -- drop until the element before it
          sumRange (start, finish) = (start, finish, sum $ map (\x -> l !! x) [start..finish])
          sumOfRanges = foldr (\x acc -> sumRange x:acc) [] ranges
          maxRange = foldr (\x@(_, _, s') acc@(_, _, s) -> if s' > s then x else acc) (head sumOfRanges) sumOfRanges

main = putStrLn "Hello World"