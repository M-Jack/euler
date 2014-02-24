module Euler.P00X (
                   p001,
                   p002,
                   p003,
                   p004,
                   p005,
                   p006,
                   p007,
                   p008,
                   p009                       
) where


import Euler.Prime
import Data.Char (digitToInt)
import Data.List (tails)

p8String :: String
p8String = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"


p001 :: Integer
p001 = sum $ filter (\x -> (mod x 5 == 0) || (mod x 3 == 0)) [1..999]

p002 :: Integer
p002 = sum $ filter (\x -> mod x 2 == 0) $ takeWhile (< 4000000) fib
    where fib = 1 : 1 : (zipWith (+) fib $ tail fib)
p003 :: Integer
p003 = last $ primeFac 600851475143 

p004 :: Integer     
p004 = maximum [x * y | x <- [100..999], y <- [100.. 999], isPal $ x * y]
     where isPal x = show x == reverse (show x)

p005 :: Integer
p005 = foldr lcm 1 [1..20]

p006 :: Integer
p006 = ((\x -> x * x) $ sum [1..100]) - (sum $ map (\x -> x * x) [1..100])

p007 :: Integer
p007 = primes !! 10000

p008 :: Integer
p008 = fromIntegral $ maximum $ map (product . take 5) $ tails $ map digitToInt p8String

p009 :: Integer
p009 = head [x * y * z | x <- [1..1000], y <-[x..1000 - x], let z = 1000 - x - y,  x * x + y * y == z * z]
