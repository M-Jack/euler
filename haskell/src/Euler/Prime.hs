module Euler.Prime (
                      isPrime,
                      primes,
                      primeFac                      
) where
      

isPrime :: Integer -> Bool
isPrime x 
    | x > 1 = all (\y -> mod x y /= 0) $ takeWhile (\y -> y * y <= x ) primes
    |otherwise = False

primes :: [Integer]
primes = 2: filter isPrime [3,5..]

primeFac :: Integer -> [Integer]
primeFac x = primeFac' x primes
    where primeFac' y (p:ps)           
              | y < p * p = [y]
              | y `mod` p == 0 = p : primeFac' (y `div` p) (p:ps)
              |otherwise = primeFac' y ps             

