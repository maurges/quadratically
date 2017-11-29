module Validate
(
    validateKey
) where

import Data.Function
import Data.Char

validateKey :: String -> String -> Bool
validateKey key name =
    let bls = g . f . f $ (key, [])
        check1 = conform bls
        check2 = length name >= 3
        check3 = checkName name bls
        nms = map ennumber bls
        a : b : c : _ = nms
        check4 = s (d a b c) m == 1
    in check1 && check2 && check3 && check4

f :: (String, [String]) -> (String, [String])
f (s, xs) = let (f,r) = break id s
              in case r of
                '-' : rest -> (rest, f:xs)
                _ -> (r, f:xs)
    where id x = x == '-'

g :: (String, [String]) -> [String]
g (s, xs) = s:xs

noDashes = notElem '-'

conform = all good
    where
        good [] = False
        good x = noDashes x

checkName (x:y:z:_) [a, b, c] =
    head a == x && head b == y && head c == z

ennumber a = foldl good_sum 0 a
    where good_sum x = (+) x . (`mod` 13) . (+ (-1) * ord 'a') . ord . toLower

m = 1000000007 :: Int

s a b 
    | a == 1
        = 1
    | a == -1
        = q b
    | a == 2
        = e b
    | a `mod` 2 == 0
        = s 2 b * s (a `div` 2) b
    | otherwise
        = dq a b * s (b `mod` a) a

e x
    | x `mod` 8 == 1 = 1
    | x `mod` 8 == 7 = 1
    | x `mod` 8 == 3 = -1
    | x `mod` 8 == 5 = -1
    | otherwise = 0


q x
    | x `mod` 4 == 1 = 1
    | x `mod` 4 == 3 = -1
    | otherwise = 0


dq a b
    | a `mod` 4 == 1 = 1
    | b `mod` 4 == 1 = 1
    | a `mod` 4 == 3 && b `mod` 4 == 3 = -1
    | otherwise = 0


d a b c = b * b - 4 * a * c
