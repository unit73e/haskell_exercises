-- file: ch04/Sum.hs
mySum xs = helper 0 xs
    where helper acc (x:xs) = helper (acc + x) xs
          helper acc _      = acc

mySum2 xs = foldl (+) 0 xs

mySum3 :: [Integer] -> Integer
mySum3 = foldl (+) 0
