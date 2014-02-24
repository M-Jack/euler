module Euler.P03X (
                   p030,
                   p031,
                   p032,
                   p033,
                   p034,
                   p035,
                   p036,
                   p037,
                   p038,
                   p039            
) where

import Data.Ord (comparing)
import Data.Char (digitToInt, intToDigit)
import Data.List
import Data.Ratio
import Data.Maybe (fromJust)
import Numeric
import Euler.Prime

p030 :: Integer
p030 = fromIntegral $ sum $ filter ok [10 .. 6 * 9 ^ 5]
    where ok x = x == (sum $ map (^ 5) $ digits x)          
          digits x = map digitToInt $ show x

p031 :: Integer
p031 = genericLength $ foldr combi [total] coins
      where coins = [200, 100, 50, 20, 10, 5, 2]
            total = 200
            combi coin currents = concat $ map possible currents
                where possible sub = map (\x -> sub - x * coin) [0 .. sub `div` coin]

p032 :: Integer
p032 = sum $ nub $ map snd $ filter (\(x, _) -> sort x == "123456789") [(output, a * b) | a <- [2..5000], b <- [2..a], let output = show a ++ show b ++ show (a * b), length output == 9]

p033 :: Integer
p033 = denominator $ product  [a % b | x <- [1..9], y <- [1..9], c <- [1..9], a <- [ 10 * x + c, 10 * c + x], b <- [10 * y + c, 10 * c + y], a * y == b * x, a < b]

p034 :: Integer
p034 = sum $ filter ok [10 .. 6 * f 9]
    where ok x = (==) x . sum . map f $ digits x          
          f x = save !! x
          save = scanl (*) 1 [1..9]
          digits x = map digitToInt $ show x
           
p035 :: Integer
p035 = genericLength $ 2 : 5 : filter (\x -> all (flip elem preList) $ shifts x) preList
    where shifts x = map (\(a,b) -> read $ b ++ a) $ map (\n -> splitAt n $ show x) [1..length $ show x]            
          preList = filter isPrime $ filter (all (flip elem [1,3,7,9]) . digits ) [2..1000000]          
          digits x = map digitToInt $ show x

p036 :: Integer
p036 = sum $ filter (\x -> isPal x && isPal (showIntAtBase 2 intToDigit x "")) [1..1000000]
      where isPal x = show x == reverse (show x)

p037 :: Integer
p037 = sum $ take 11 $ filter ok [10..]
      where ok x = all isPrime (ltor x ++ rtol x)
            ltor x = map read $ init $ tails $ show x
            rtol x = map read $ tail $ inits $ show x

p038 :: Integer
p038 = read $ maximum $ filter isPandigit $ map generate [1 .. 9999]
      where generate x = fromJust $ find (\x -> length x > 8) $  scanl1 (++) $ prod x
            prod x = map show $ map (x *) [1..]
            isPandigit x = sort x == "123456789"
            
p039 :: Integer
p039 = fst $ maximumBy (comparing snd) $ map perimeters [1..1000]
      where perimeters x = (x, length [(a,b,c)| a <- [1..x `div` 3], b <- [a.. (x - a) `div` 2], let c = x - a - b, a * a + b * b == c * c])
