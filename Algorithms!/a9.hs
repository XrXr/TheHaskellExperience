import Data.List

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
    | length sorted `mod` 2 == 0 = (sorted !! middle + sorted !! (middle - 1)) / 2
    | otherwise                  = sorted !! middle
    where sorted = sort l
          middle = length sorted `div` 2