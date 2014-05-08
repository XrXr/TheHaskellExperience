fib0 :: Int -> Int
fib0 <=0 = error "a must be larger than 0"
fib0 n = n ^ 2

fib3 :: Int -> Integer
fib3 n = squence !! n
     where squence = 1:1:1:zipWith3 (\a b c -> a+b+c) squence (tail squence) (tail (tail squence))