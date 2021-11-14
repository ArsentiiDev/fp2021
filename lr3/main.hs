
import Data.List
--Завдання 1 
--а)
insert :: Int -> a -> [a] -> [a]
insert n y xs = countdown n xs where
   countdown 0 xs = y:countdown n xs -- reset to original n
   countdown _ [] = []
   countdown m (x:xs) = x:countdown (m-1) xs

--б)
insertAtN :: Int -> a -> [a] -> [a]
insertAtN n y  = intercalate [y] . groups n
  where
    groups n = takeWhile (not.null) . unfoldr (Just . splitAt n)

--Завдання 2
--а)
nPrimes x y =  head [z | z <- [x..y], isPrime z]
isPrime :: (Integral a) => a -> Bool
isPrime k | k <=1 = False | otherwise = 0 `notElem`  moding k [2..k-1]

moding _ [] = []
moding k (x:xs) = mod k x: moding k xs


--б) 
--nPrimes :: Integer -> Integer -> Integer 
nPrimes2 x y =  head [z | z <- [x..y], isPrime z]

isPrime2 :: (Integral a) => a -> Bool
isPrime2 k | k <=1 = False | otherwise = 0 `notElem` map (mod k)[2..k-1]











