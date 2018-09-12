import Fizzbuzz

main = do
    mapM_ (putStrLn . evaluateRules) [1..100]
