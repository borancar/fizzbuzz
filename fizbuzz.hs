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

printRule :: Integer -> Maybe String
printRule i = Just $ show i

evaluateRules :: Integer -> String
evaluateRules i = fromJust $ msum $ sequence rules i where
    rules = [fizzBuzzRule, fizzRule, buzzRule, printRule]

main = do
    mapM_ (putStrLn . evaluateRules) [1..100]
