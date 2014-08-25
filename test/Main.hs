module Main where

import           Test.Tasty

import qualified Clerk      as Clerk

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Clerk.tests]

{-module Main where-}

{-import           Test.Tasty-}
{-import           Test.Tasty.HUnit-}
{-import           Test.Tasty.Options-}
{-import           Test.Tasty.QuickCheck as QC-}
{-import           Test.Tasty.SmallCheck as SC-}

{-import           Data.List-}
{-import           Data.Ord-}

{-main :: IO ()-}
{-main = defaultMain tests-}

{-tests :: TestTree-}
{-tests = testGroup "Tests" [properties, unitTests]-}

{-properties :: TestTree-}
{-properties = testGroup "Properties" [scProps, qcProps]-}

{-scProps = localOption (SmallCheckDepth 5) $ testGroup "(checked by SmallCheck)"-}
    {-[ SC.testProperty "sort == sort . reverse" $-}
        {-\list -> sort (list :: [Int]) == sort (reverse list)-}
    {-, SC.testProperty "Fermat's little theorem" $-}
        {-\x -> ((x :: Integer)^7 - x) `mod` 7 == 0-}
    {-, SC.testProperty "Fermat's last theorem" $-}
        {-\x y z n ->-}
            {-(n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)-}
    {-]-}

{-qcProps = localOption (QuickCheckTests 1000) $ testGroup "(checked by QuickCheck)"-}
    {-[ QC.testProperty "sort == sort . reverse" $-}
        {-\list -> sort (list :: [Int]) == sort (reverse list)-}
    {-, QC.testProperty "Fermat's little theorem" $-}
        {-\x -> ((x :: Integer)^7 -x ) `mod` 7 == 0-}
    {-, QC.testProperty "Fermat's last theorem" $-}
        {-\x y z n ->-}
            {-(n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)-}
    {-]-}

{-unitTests = testGroup "Unit tests"-}
    {-[ testCase "List comparison (different length)" $-}
        {-[1, 2, 3] `compare` [1, 2] @?= GT-}
    {-, testCase "List comparison (same length)" $-}
        {-[1, 2, 3] `compare` [1, 2, 2] @?= LT-}
    {-]-}
