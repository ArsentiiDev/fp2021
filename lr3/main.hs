
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
isprime :: [Int] -> [Int]
isprime [x] |x==2 =[x]
isprime [] = []
isprime k |[x| x <- (x:xs), mod k x]
                   | otherwise = isprime xs