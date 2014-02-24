module Euler.P04X (
                   p040,
                   p041,
                   p042,
                   p043,
                   p044,
                   p045,
                   p046,
                   p047,
                   p048,
                   p049
) where

import Data.Char (digitToInt, ord)
import Data.List
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import System.IO.Unsafe
import Euler.Prime

p040 :: Integer
p040 = fromIntegral $ product $ map d [1, 10, 100, 1000, 10000, 100000, 1000000]
      where d x = digitToInt $ string !! (x - 1)
            string = concat $ map show [1..]

p041 :: Integer
p041 = maximum $ filter isPrime $ map read $ concat $ map permutations $ init $ tails "987654321"

p042 :: Integer
p042 = genericLength $ filter (isTri . value) parse    
       where str = unsafePerformIO $ readFile "../words.txt"    
             parse = words $ map (\x-> if x == ',' then ' ' else x)  str    
             value x = sum $ map (\c -> 1 + ord c - ord 'A') $ read x 
             tri x = x * (x + 1) `div` 2
             isTri x  = x == tri (floor $ sqrt $ fromIntegral $ 2 * x)
    

p043 :: Integer
p043 = sum $ map read $ filter ok $ permutations "1234567890"
      where ok xs = check 2 2 && check 3 3 && check 4 5 && check 5 7 && check 6 11 && check 7 13 && check 8 17
                where check x y = (read $ take 3 $ drop (x - 1) xs) `rem` y == 0

p044 :: Integer
p044 = head $ catMaybes [choice a b | x <- [1..3000], y <- [1..x], let a = pentagon x, let b = pentagon y, isPentagon $ a + b]
    where pentagon x = x * (3 * x - 1) `div` 2          
          isPentagon x = let n = ceiling $ sqrt $ 2 * (fromIntegral x) / 3 
                         in x == pentagon n
          choice x y
              | isPentagon $ 2 * x + y = Just y
              | isPentagon $ 2 * y + x = Just x
              | otherwise = Nothing

p045 :: Integer
p045 = [x | x <- map hex [1..], isPentagon x, isTriangle x] !! 2
      where triangle x = x * (x + 1) `div` 2
            pentagon x = x * (3 * x - 1) `div` 2
            hex x = x * ( 2 * x - 1)
            isTriangle x = let n = floor $ sqrt $ fromIntegral $ 2 * x in 
                           x == triangle n
            isPentagon x = let n = ceiling $ sqrt $ 2 * (fromIntegral x) / 3 in
                           x == pentagon n

p046 :: Integer
p046 = head $ filter ok $ filter (not . isPrime) [3,5..]
      where ok x = all (\y ->  not $ isSq $ (x - y) `div` 2) $ takeWhile (< x) primes
            isSq x = let n = floor $ sqrt $ fromIntegral x in
                     n * n == x

p047 :: Integer            
p047 = search foured
      where search (x : ys @ (y : zs @ (z : ws @ (w : _))))
                |z + 1 /= w = search $ ws
                |y + 1 /= z = search $ zs
                |x + 1 /= y = search $ ys
                |otherwise = x
            foured = filter ok [2..]
            ok x = length (nub $ primeFac x) == 4

p048 :: Integer
p048 = trunc $ sum $ map f [1..1000]
    where f x = pow x x 1
          pow e b acc
              | e == 1 = trunc $ b * acc
              | e `rem` 2 == 0 = pow (e `div` 2) (trunc $ b * b) acc
              |otherwise = pow (e `div` 2) (trunc $ b * b) (trunc $ trunc b * acc)             
          trunc x = x `rem` 10000000000
      
p049 :: Integer
p049 = [read $ show x ++ show y ++ show z | set <- sets, x <- set, y <- set, z <- set, x < y, y < z, z - y == y - x] !! 1
    where sets = map (map snd) $ groupBy grouper $ sort $ map (\x -> (sort $ show x, x)) $ filter isPrime [1000..9999]
          grouper (x,_) (y,_) = x == y
