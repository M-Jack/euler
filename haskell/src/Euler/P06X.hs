module Euler.P06X (
                   p060,
                   p061,
                   p062,
                   p063,
                   p064,
                   p065,
                   p066,
                   p067,
                   p068,
                   p069
) where

import Data.List
import Control.Monad

import Euler.Prime

p060 :: Integer          
p060 = sum $ search [] $ tail primes
       where possible x = filter (edge x) $ takeWhile (< x) $ tail  primes
             edge x y = (isPrime $ read (show x ++ show y)) && (isPrime $ read (show y ++ show x))
             solution neigh square = filter (all (flip elem neigh)) square
             search square (p:ps) = if null sol then search square' ps else p : head sol
                 where neigh = possible p
                       sol = solution neigh square                                             
                       square' = [[p,c,b,a]|
                                  c <- neigh, 
                                  b <- neigh,
                                  b < c,
                                  edge b c,
                                  a <- neigh,                                  
                                  a < b,
                                  edge a b,
                                  edge a c
                                 ] ++ square

p061 :: Integer
p061 = sum $ concat $ concat $ 
      map choose $ map (octogons :) $ permutations [squares, pentagons, hexagons, heptagons, triangles]
    where lists f = dropWhile (< 1000) $ takeWhile (<10000) $ map f [1..]
          triangles = lists (\x -> x * (x + 1) `div` 2)
          squares = lists (\x -> x * x)
          pentagons = lists (\x -> x * (3 * x - 1) `div` 2)
          hexagons = lists (\x -> x * (2 * x - 1))
          heptagons = lists (\x -> x * (5 * x - 3) `div` 2)
          octogons = lists (\x -> x * (3 * x - 2))
          following x y = drop 2 (show x) == take 2 (show y)
          choose [a,b,c,d,e,f] = do 
            m <- a
            n <- b
            guard $ following m n     
            o <- c
            guard $ following n o
            p <- d
            guard $ following o p
            q <- e
            guard $ following p q
            r <- f
            guard $ following q r
            guard $ following r m
            return [m,n,o,p,q,r]
            
p062 :: Integer
p062 = head $ filter ok [1..]
    where ok x = length (fellows $ x ^ 3) == 2
          fellows x = filter isCurt $ nub $ filter (>x) $ map read $ permutations $ show x
          cubes = map (^3) [1..]
          isCurt x = round (fromIntegral x ** (1/3)) ^ 3 == x

p063 :: Integer
p063 = undefined

p064 :: Integer
p064 = undefined

p065 :: Integer
p065 = undefined

p066 :: Integer
p066 = undefined

p067 :: Integer
p067 = undefined

p068 :: Integer
p068 = undefined

p069 :: Integer
p069 = undefined

