import Control.Monad
import Data.Maybe

printRule :: Integer -> Maybe String
printRule i = Just $ show i

evaluateRules :: Integer -> String
evaluateRules i = fromJust $ msum $ sequence rules i where
    rules = [printRule]

main = do
    mapM_ (putStrLn . evaluateRules) [1..100]
