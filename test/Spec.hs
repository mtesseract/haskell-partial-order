module Main where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.List
import Data.Ord
#if EXTRA_INSTANCES
import qualified Data.Set as S
#endif
import qualified Data.PartialOrd as PO
import Test.HUnit ((@?=))

main :: IO ()
main = defaultMain tests

tests :: [Test.Framework.Test]
tests =
  [ testGroup "Number Properties"
    [
      testProperty "Int"
      (prop_num :: Integer -> Integer -> Bool)
    , testProperty "Integer"
      (prop_num :: Int     -> Int     -> Bool)
    , testProperty "Double"
      (prop_num :: Double  -> Double  -> Bool)
    , testProperty "Float"
      (prop_num :: Float   -> Float   -> Bool)
    ]
  , testGroup "List Properties"
    [
      testProperty "=="
      (\ a b -> compareBinFuns (equal isSublistOf) (PO.==)
                               (a :: [Int]) (b :: [Int]))
    , testProperty "== (sort)"
      (\ a -> let a' = sort a :: [Int]
              in compareBinFuns (equal isSublistOf) (PO.==)
                                (reverse a') a')
    , testProperty "/="
      (\ a b -> compareBinFuns (notEqual isSublistOf) (PO./=)
                               (a :: [Int]) (b :: [Int]))
    , testProperty "<="
      (\ a b -> compareBinFuns isSublistOf (PO.<=)
                               (a :: [Int]) (b :: [Int]))
    , testProperty ">="
      (\ a b -> compareBinFuns (geq isSublistOf) (PO.>=)
                               (a :: [Int]) (b :: [Int]))
    , testProperty "<"
      (\ a b -> compareBinFuns (less isSublistOf) (PO.<)
                               (a :: [Int]) (b :: [Int]))
    , testProperty ">"
      (\ a b -> compareBinFuns (greater isSublistOf) (PO.>)
                               (a :: [Int]) (b :: [Int]))
    , testProperty "transitivity"
      (\ a b c -> prop_trans (a :: [Int])
                             (b :: [Int])
                             (c :: [Int]))
    , testProperty "antisymmetry"
      (\ a b -> prop_antisymmetry (a :: [Int]) (b :: [Int]))
    ]
#if EXTRA_INSTANCES
  , testGroup "Set Properties"
    [
      testProperty "=="
      (\ a b -> compareBinFuns (equal S.isSubsetOf) (PO.==)
                (a :: S.Set Int) (b :: S.Set Int))
    , testProperty "/="
      (\ a b -> compareBinFuns (notEqual S.isSubsetOf) (PO./=)
                               (a :: S.Set Int) (b :: S.Set Int))
    , testProperty "<="
      (\ a b -> compareBinFuns S.isSubsetOf (PO.<=)
                               (a :: S.Set Int) (b :: S.Set Int))
    , testProperty ">="
      (\ a b -> compareBinFuns (geq S.isSubsetOf) (PO.>=)
                               (a :: S.Set Int) (b :: S.Set Int))
    , testProperty "<"
      (\ a b -> compareBinFuns (less S.isSubsetOf) (PO.<)
                               (a :: S.Set Int) (b :: S.Set Int))
    , testProperty ">"
      (\ a b -> compareBinFuns (greater S.isSubsetOf) (PO.>)
                               (a :: S.Set Int) (b :: S.Set Int))
    , testProperty "transitivity"
      (\ a b c -> prop_trans (a :: S.Set Int)
                             (b :: S.Set Int)
                             (c :: S.Set Int))
    , testProperty "antisymmetry"
      (\ a b -> prop_antisymmetry (a :: S.Set Int) (b :: S.Set Int))
    ]
#endif
  , testGroup "Maxima & Minima"
    [
      testProperty "maxima exist"
      (prop_extrema_exist (PO.maxima :: [Int] -> [Int]))
    , testProperty "minima exist"
      (prop_extrema_exist (PO.minima :: [Int] -> [Int]))
    , testProperty "minima are minimal"
      (prop_extrema_extremal (PO.minima :: [[Int]] -> [[Int]]) isSuplistOf)
    , testProperty "maxima are maximal"
      (prop_extrema_extremal (PO.maxima :: [[Int]] -> [[Int]]) isSublistOf)
    , testProperty "Unique maximum for Ord types"
      (prop_unique_extremum (PO.maxima :: [Int] -> [Int]) maximum)
    , testProperty "Unique minimum for Ord types"
      (prop_unique_extremum (PO.minima :: [Int] -> [Int]) minimum)
    ]
  , testGroup "Known Extrema"
    (map (\ (idx, extrema) ->
             let label = "extremal cases (" ++ show idx ++ ")"
             in testCase label (test_known_extrema extrema))
      (zip [1..] knownExtrema))
  ]

test_known_extrema :: PO.PartialOrd a => ([a], [a], [a]) -> IO ()
test_known_extrema (as, asMax, asMin) =
  ((equal isSublistOf) (PO.maxima as) asMax
    && (equal isSublistOf) (PO.minima as) asMin) @?= True

knownExtrema :: [([[Int]], [[Int]], [[Int]])]
knownExtrema = [ ( [ [], [1, 2, 3], [4, 5], [], [4, 5]
                   , [3, 4], [0], [0, 1, 2, 3, 4, 6] ]
                 , [ [ 0, 1, 2, 3, 4, 6], [4, 5] ]
                 , [ [] ] )
               , ( [ [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] ]
                 , [ [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] ]
                 , [ [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] ] )
               , ( []
                 , []
                 , [] )
               , ( [ [ 1, 2 ], [ 2, 1 ] ]
                   , [ [ 1, 2 ] ]
                   , [ [ 1, 2 ] ] )
               , ( [ [ 1, 2 ], [ 2, 3 ] ]
                   , [ [ 1, 2 ], [ 2, 3 ] ]
                   , [ [ 1, 2 ], [ 2, 3 ] ] )
               , ( [ [ 0 ], [ 0, 0 ] ]
                 , [ [ 0 ] ]
                 , [ [ 0 ] ] )
               , ( [ [ 1 ], [ 1 ], [ 1 ] ]
                 , [ [ 1 ] ]
                 , [ [ 1 ] ] )
               , ( [ [ ], [ -1, -2, -3 ] ]
                 , [ [ -3, -2, -1 ] ]
                 , [ [ ] ] )
               ]

prop_trans :: PO.PartialOrd a => a -> a -> a -> Bool
prop_trans a b c =
  case (a PO.<= b, b PO.<= c) of
    (True, True) -> a PO.<= c
    _ -> True

prop_antisymmetry :: PO.PartialOrd a => a -> a -> Bool
prop_antisymmetry a b =
  if a PO.<= b
  then a PO.== b || not (a PO.>= b)
  else True
  
prop_unique_extremum :: Ord a => ([a] -> [a]) -> ([a] -> a) -> [a] -> Bool
prop_unique_extremum _ _ [] = True
prop_unique_extremum computeExtrema computeExtremum as =
  case computeExtrema as of
    [extremum] -> extremum == computeExtremum as
    _ -> False
  
prop_extrema_extremal :: Eq a =>
                         ([a] -> [a]) -> (a -> a -> Bool) -> [a] -> Bool
prop_extrema_extremal computeExtrema relation as =
  let extrema  = computeExtrema as
      extrema' = filter (isBiggerExtremum extrema) as
  in null extrema'
  where -- Returns True if a < a' where a' is in extrema.
        isBiggerExtremum extrema a =
          notNull $ filter (\ e -> (less relation) e a) extrema

        notNull = not . null

prop_extrema_exist :: Eq a => ([a] -> [a]) -> [a] -> Bool
prop_extrema_exist f as =
  null as || (not . null) (f as)

prop_num :: (Num a, Ord a) => a -> a -> Bool
prop_num x y =
  case x `compare` y of
    LT -> x < y
    GT -> x > y
    EQ -> x == y

-- Check if two given binary functions agree on the given input.
compareBinFuns :: Eq c =>
                  (a -> b -> c) -> (a -> b -> c) -> a -> b -> Bool
compareBinFuns f g a b = (a `f` b) == (a `g` b)

-- Implement equality given less-or-equal relation.
equal :: (a -> a -> Bool) -> a -> a -> Bool
equal leq a b = a `leq` b && b `leq` a

-- Implement inequality given less-or-equal relation.
notEqual :: (a -> a -> Bool) -> a -> a -> Bool
notEqual leq a b = not $ equal leq a b

-- Implement greater-or-equal given a less-or-equal relation.
geq :: (a -> a -> Bool) -> a -> a -> Bool
geq = flip

-- Implement strictly-less given a less or-equal relation.
less :: (a -> a -> Bool) -> a -> a -> Bool
less leq a b = (a `leq` b) && notEqual leq a b

  -- Implement strictly-greater given less-or-equal relation.
greater :: (a -> a -> Bool) -> a -> a -> Bool
greater leq a b = less leq b a

-- Check if each element of the first list is also an element of the
-- second list.
isSublistOf :: PO.PartialOrd a => [a] -> [a] -> Bool
isSublistOf [] bs = True
isSublistOf (a:as) bs = a `PO.elem` bs && as `isSublistOf` bs

-- Check if each element of the second list is also an element of the
-- first list.
isSuplistOf :: PO.PartialOrd a => [a] -> [a] -> Bool
isSuplistOf = flip isSublistOf
