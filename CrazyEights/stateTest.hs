import Control.Monad.State

dum :: Int -> State Int Int
dum a = state $ (\x -> (x+1, x + a))

meow :: State Int Int
meow = do
    dum 60
    dum 60
    dum 60
    dum 60
    dum 60
    dum 60
    dum 60
    
removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt i l  = begin ++ drop 1 end
    where (begin, end) = splitAt i l
    
    
hap = if False then
        "asdasd"
      else
        if True then
            "succeess"
        else
            "fail"

mote = a + b
    where b = a + 100
          a = 10