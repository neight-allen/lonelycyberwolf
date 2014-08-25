module Clerk
    ( tests
    ) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC

import           Actor.Clerk

tests :: TestTree
tests = testGroup "Clerk" [testMatching]

testMatching :: TestTree
testMatching = [ QC.testProperty "If an ask cannot be filled, no bids above its price should remain."  prop_NoEscapedBids
               ]

{-prop_NoEscapedBids :: Order -> [Order] -> Property-}
{-prop_NoEscapedBids o os = undefined-}
prop_NoEscapedBids o = forAll . orderedList $ \os -> undefined
    where (mo, bts, ms) = match o os
