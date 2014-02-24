module Euler.P05X (
                   p050,
                   p051,
                   p052,
                   p053,
                   p054,
                   p055,
                   p056,
                   p057,
                   p058,
                   p059
) where

import Data.Maybe
import Data.List
import Control.Monad
import Data.Ord
import Data.Char
import Data.Ratio
import Data.Bits
import System.IO.Unsafe

import Euler.Prime          

p050 :: Integer
p050 = search longest 0
      where longest = length $ takeWhile (<1000000) $ scanl (+) 0 primes            
            search x y
                |s > 1000000 = search (x - 1) 0
                |isPrime s = s
                |otherwise = search x $ y + 1
                where s = sum $ take x $ drop y primes



p051 :: Integer
p051 = read $ fst $ fromJust $ find (\x -> familly x == 8) candidates
      where triple x = find ((<=) 3 . length) $ group $ sort x             
            candidates = catMaybes $  map (\x -> liftM (\y -> (x, head  y)) $ triple x) $ map show primes
            familly (prime, target)
                |head prime /= target = length $ filter (isPrime . read) $ map (\x -> map (\y -> if y == target then x else y) prime) ['0'..'9']
                |otherwise = length $ filter (isPrime . read) $ map (\x -> map (\y -> if y == target then x else y) prime) ['1'..'9']

p052 :: Integer
p052 = fromJust $ find ok [1..]
      where ok x = (==) 1 $ length $ group $ map (sort . show . (x *)) [1..6]

p053 :: Integer
p053 = genericLength $ filter ( > 1000000) [comb n r | n <- [1..100], r <- [1..n]]
      where comb n r = product [r + 1 .. n] `div` product [2.. n - r]

data Suit = Club | Spade | Heart | Diamond deriving (Show, Eq)

data Kind = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord, Enum)

data Card = Card {kind :: Kind, suit :: Suit}

instance Read Card where
    readsPrec _ (k:s:xs) = [(Card (toKind k) (toSuit s), xs)]
        where toKind '2' = Two
              toKind '3' = Three
              toKind '4' = Four
              toKind '5' = Five
              toKind '6' = Six
              toKind '7' = Seven
              toKind '8' = Eight
              toKind '9' = Nine
              toKind 'T' = Ten
              toKind 'J' = Jack
              toKind 'Q' = Queen
              toKind 'K' = King
              toKind 'A' = Ace
              toSuit 'C' = Club
              toSuit 'S' = Spade
              toSuit 'H' = Heart
              toSuit 'D' = Diamond

instance Show Card where
    showsPrec p (Card k s) = showParen (p > 10) $
                     shows k .  showString " " . shows s
    
type Hand = [Card]

data Rank = HighCard {hKind :: Kind} | 
    Pair {pairKind :: Kind, hKind :: Kind} | 
    DoublePair {fstKind :: Kind, sndKind :: Kind, hKind :: Kind} |
    ThreeOfAKind {tripleKind :: Kind, hKind :: Kind} |
    Straight {hKind :: Kind} |
    Flush {hKind :: Kind} |
    FullHouse {tripleKind :: Kind, pairKind :: Kind} |
    FourOfAKind {fourKind :: Kind} |
    StraightFlush {hKind :: Kind} |
    RoyalFlush 
    deriving (Show, Eq, Ord)

rank :: Hand -> Rank
rank hand
    |isRoyalFlush = RoyalFlush
    |isStraightFlush = StraightFlush highest
    |isFourOfAKind = FourOfAKind most
    |isFullHouse = FullHouse most less
    |isFlush = Flush highest
    |isStraight = Straight highest
    |isThreeOfAKind = ThreeOfAKind most highest
    |isDoublePair = DoublePair fstPair sndPair less
    |isPair = Pair most highest
    |otherwise = HighCard highest
    where kinds = group $ reverse $ sort $ map kind hand
          scheme = sort $ map length kinds
          highest = head $ head $ filter (\x -> length x == 1) kinds
          most = head $ last $ sortBy (comparing length) kinds
          less = head $ head $ sortBy (comparing length) kinds
          fstPair = head $ head $ filter (\x -> length x == 2) kinds
          sndPair = head $ last $ filter (\x -> length x == 2) kinds
          isRoyalFlush = isStraightFlush && highest == Ace
          isStraightFlush = isFlush && isStraight
          isFourOfAKind = scheme == [1,4]
          isFullHouse = scheme == [2,3]
          isFlush = (length $ group $ map suit hand) == 1
          isStraight = map head kinds == take 5 (iterate pred highest)
          isThreeOfAKind = scheme == [1,1,3]
          isDoublePair = scheme == [1,2,2]
          isPair = scheme == [1,1,1,2]
          
          
p054 :: Integer
p054 = genericLength $ filter (\(x, y) -> rank x > rank y) games
    where str = unsafePerformIO $ readFile "../poker.txt"
          games = map (splitAt 5 . map read) $ map words $ lines str 
    

p055 :: Integer
p055 = genericLength $ filter lychrel [1..10000]
      where lychrel x = all (not . isPal) $ take 50 $ tail $ iterate f x 
            f x = x + (read $ reverse $ show x)
            isPal x = show x == reverse (show x)

p056 :: Integer
p056 = maximum $ [fromIntegral $ digSum $ a ^ b | a <- [1..100], b <- [1..100]]
      where digSum x = sum $ map digitToInt $ show x
    
p057 :: Integer
p057 = genericLength $ filter ok $ take 1000 $ iterate (\x -> 1 + 1 / (1 + x)) $ 3 % 2
      where ok x = (length $ show $ numerator x) > (length $ show $ denominator x)

p058 :: Integer
p058 = search 1 3
      where search state prime
                |10 * prime < 4 * state + 1 = 2 * state + 1
                |otherwise = search (state + 1) $ prime + new (state + 1)            
            new x = let n = (2 * x + 1) ^ 2 in
                    genericLength $ filter isPrime [n - 2 * x, n - 4 * x, n - 6 * x]

p059 :: Integer
p059 = fromIntegral $ sum $ map ord $ head $ filter (all ( \x -> notElem x "\DEL\"/~%$")) $ map (decipher cipher) [[a,b,c] | let lc = map ord ['a'..'z'], a <- lc, b <- lc, c <- lc]
    where cipher = unsafePerformIO $ liftM (read . (\x -> '[' : (x ++ "]"))) $ readFile "../cipher1.txt"     
          decipher cipher key = map chr $ zipWith xor cipher $ cycle key
