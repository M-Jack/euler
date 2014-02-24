module Euler.P02X (
                   p020,
                   p021,
                   p022,
                   p023,
                   p024,
                   p025,
                   p026,
                   p027,
                   p028,
                   p029
) where

import Data.Char (digitToInt, ord)
import Data.List
import Data.Ord (comparing)
import Data.Maybe (fromJust)
import System.IO.Unsafe
import Euler.Prime

p020 :: Integer
p020 = fromIntegral $ sum $ map digitToInt $ show $ product [1..100]      

p021 :: Integer
p021 = sum $ takeWhile (< 10000) amicables
      where amicable x = let friend = sum $ divisors x in (sum $ divisors friend) == x && x /= friend
            amicables = filter amicable [2 .. ]
            divisors x = filter (\y -> mod x y == 0) [1.. x `div` 2]

p022 :: Integer
p022 = fromIntegral $ sum $ map (\(x, y) -> x * value y) $ zip [1..] $ sort parse    
       where str = unsafePerformIO $ readFile "../names.txt"    
             parse = words $ map (\x-> if x == ',' then ' ' else x)  str    
             value x = sum $ map (\y -> 1 + ord y - ord 'A') $ read x 


p023 :: Integer
p023 = sum $ filter (not .abSum)  [1..28123]
       where abundant x = (<) x . sum $ filter ((==) 0 . mod x) [1.. x `div` 2]                               
             abundants = filter abundant $ filter (\x -> even x || mod x 3 == 0) [12..28123]
             abSum x = search $ takeWhile (<x) abundants
                 where search ls = step ls $ reverse ls
                       step [] _ = False
                       step _ [] = False
                       step (y: ys) (z:zs) 
                           |y + z == x = True
                           |y + z > x = step (y:ys) zs
                           |otherwise = step ys (z:zs)


p024 :: Integer
p024 = read $ (sort $ permutations "0123456789") !! 999999

p025 :: Integer
p025 = fst $ head $ filter (\(y, x) -> (length $ show x) >= 1000) $ zip [1..] fib
      where fib = 1 : 1 : (zipWith (+) fib $ tail fib)

      
p026 :: Integer
p026 = maximumBy (comparing recCycle) $ filter (\x -> odd x && mod x 5 /= 0) [1..1000]
      where recCycle x = 1 + (fromJust $ findIndex (\n -> mod n x == 0) $ iterate (\x -> 10 * x + 9) 9)

p027 :: Integer
p027 = snd . maximum $ map (\(x, y) -> (primeProd x y, x * y))  [(x, y) | y <- [2.. 999], isPrime y, x <- [(1 - y) .. 999]]
      where primeProd a b = length $ takeWhile (test a b) [1..]
            test a b x = isPrime ((x + a) * x + b )   

p028 :: Integer
p028 = 1001 * 1001 + sum  [ x | y <- [1..500], let x = 16 * y * y - 4 * y + 4] 

p029 :: Integer
p029 = genericLength $ nub [a ^ b | a <- [2..100], b <- [2..100]]
      


