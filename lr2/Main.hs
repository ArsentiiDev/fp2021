import Data.List
--завдання 1
--a)
uniq2 :: [Int] -> [Int]
uniq2 [] = []
uniq2 (x:xs) = x: uniq2 (remove x xs)
    where
        remove :: Int -> [Int] -> [Int]
        remove x [] = []
        remove x (y:ys)
                        | x==y = remove x ys
                        | otherwise = y:remove x ys

--b)
uniq :: (Eq b, Ord b) => [b] -> [b]
uniq = map head.group.sort

--завдання 2 
--а)
nsk :: Float -> Float -> Float
nsk 0 _ = 0
nsk _ 0 = 0
nsk x y = let gcd x y
                    | x > y = gcd (x-y) y
                    | x < y = gcd x (y-x)
                    | otherwise = x
            in x*y / gcd x y


--б)
lcm :: (Integral a) => a -> a -> a
lcm _ 0 =  0
lcm 0 _ =  0
lcm x y =  abs (x * y `quot` gcd x y )