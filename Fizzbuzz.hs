module Fizzbuzz where

import Control.Monad
import Data.Maybe

fizzRule :: Integer -> Maybe String
fizzRule i =
    if i `mod` 3 == 0
        then Just "Fizz"
        else Nothing

buzzRule :: Integer -> Maybe String
buzzRule i =
    if i `mod` 5 == 0
        then Just "Buzz"
        else Nothing

fizzBuzzRule :: Integer -> Maybe String
fizzBuzzRule i =
    if i `mod` 3 == 0 && i `mod` 5 == 0
        then Just "FizzBuzz"
        else Nothing

-- from https://stackoverflow.com/questions/2807686/whats-the-way-to-determine-if-an-int-is-a-perfect-square-in-haskell
-- importing a library seemed too much for now so just going to use this (which was incidentally my original idea)
isSquare :: Integer -> Bool
isSquare n = sq * sq == n where
    sq = floor $ sqrt $ (fromIntegral n::Double)

isFibonacci :: Integer -> Bool
isFibonacci n = isSquare (5 * n*n + 4) || isSquare (5 * n*n - 4)

flamingoRule :: Integer -> Maybe String
flamingoRule i =
    if isFibonacci i
        then Just "Flamingo"
        else Nothing

pinkFlamingoRule :: Integer -> Maybe String
pinkFlamingoRule i =
    if isFibonacci i && i `mod` 3 == 0 && i `mod` 5 == 0
        then Just "Pink Flamingo"
        else Nothing

printRule :: Integer -> Maybe String
printRule i = Just $ show i

evaluateRules :: Integer -> String
evaluateRules i = fromJust $ msum $ sequence rules i where
    rules = [pinkFlamingoRule, flamingoRule, fizzBuzzRule, fizzRule, buzzRule, printRule]
