import Control.Monad
import Fizzbuzz
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test

genFibonacci :: Gen Integer
genFibonacci = (arbitrary :: Gen Integer) `suchThat` isFibonacci

genNotFibonacci :: Gen Integer
genNotFibonacci = (arbitrary :: Gen Integer) `suchThat` (not . isFibonacci)

genDivisibleBy :: Integer -> Gen Integer
genDivisibleBy n = (arbitrary :: Gen Integer) `suchThat` (\x -> x > 0 && x `mod` n == 0)

genNotDivisibleBy :: Integer -> Gen Integer
genNotDivisibleBy n = (arbitrary :: Gen Integer) `suchThat` (\x -> x > 0 && x `mod` n /= 0)

prop_flamingoRule a i = flamingoRule i == a
prop_fizzBuzzRule a i = fizzBuzzRule i == a

main = do
    let tests = [ quickCheckResult $ forAll genFibonacci $ prop_flamingoRule (Just "Flamingo")
                , quickCheckResult $ forAll genNotFibonacci $ prop_flamingoRule (Nothing)
                , quickCheckResult $ forAll (genDivisibleBy (3*5)) $ prop_fizzBuzzRule (Just "FizzBuzz")
                , quickCheckResult $ forAll (genNotDivisibleBy (3*5)) $ prop_fizzBuzzRule (Nothing)]

    success <- fmap (all isSuccess) . sequence $ tests
    when (not success) $ exitFailure
