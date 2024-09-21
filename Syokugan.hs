module Syokugan where

import Data.MemoTrie (memo2)
import Data.Ratio ((%))

factorial :: Integer -> Integer
factorial n = product [1..n]

combi :: Integer -> Integer -> Integer
combi n r = fn `div` (fr * fm)
  where
    fn = factorial n
    fr = factorial r
    fm = factorial $ n - r

solve :: Integer -> Integer -> Integer -> Integer -> Rational
solve nn ss rr kk = memoizedSolve rr kk
  where
    memoizedSolve = memo2 solve'
    rcombi a b = toRational $ combi a b
    solve' :: Integer -> Integer -> Rational
    solve' 0 0 = 1 % 1 
    solve' r 0 = 0 % 1 
    solve' 0 k = 0 % 1 
    solve' r k   
      | (r * ss) < k = 0 % 1
      | ss > k = 0 % 1
      | otherwise = result
          where
            c = sum $ map ft [0..ss]
            ft i = memoizedSolve (r-1) (k-i)
                   * rcombi (k-i) (ss-i)
                   * rcombi (nn - (k - i)) i
            result = c / (rcombi nn ss) 
