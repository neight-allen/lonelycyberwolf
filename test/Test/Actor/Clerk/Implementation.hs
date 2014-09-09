module Test.Actor.Clerk.Implementation where

import           Control.Applicative
import           Control.Monad.State
import           Data.IxSet
import           Test.Tasty                 as Test
import           Test.Tasty.QuickCheck      as QC

import           Actor.Clerk.Implementation
import           Actor.Types

import           Test.Actor.Types.Arbitrary

tests :: Test.TestTree
tests = Test.testGroup "Clerk" [testMatching]

testMatching :: Test.TestTree
testMatching = Test.testGroup "Matching"
        [ QC.testProperty "If an ask cannot be filled, it should have a remaining quantity."    prop_NoEmptyAsk
        , QC.testProperty "If an ask cannot be filled, no bids above its price should remain."  prop_NoEscapedBids
        , QC.testProperty "If a bid cannot be filled, it should have a remaining quantity."     prop_NoEmptyBid
        , QC.testProperty "If a bid cannot be filled, no asks below its price should remain."   prop_NoEscapedAsks
        ]

prop_NoEmptyAsk :: Ask -> OrderBook -> Bool
prop_NoEmptyAsk a@(Order _ _ ap _) ob = let (ma', (_, _)) = runState (matchAsk' a bids) (ob, [])
                                         in case ma' of
                                                Just (Order _ _ _ aq') -> aq' > 0
                                                Nothing                -> True
    where bids = takeWhile (\(Order _ _ bp _) -> ap < bp) $
                    toDescList (Proxy :: Proxy Price) (bidBook ob)

prop_NoEscapedBids :: Ask -> OrderBook -> Bool
prop_NoEscapedBids a@(Order aid _ _ _) ob = case (as, bs) of
                                                (                 [],                  _) -> True
                                                (                  _,                 []) -> True
                                                ([(Order _ _ ap' _)], [(Order _ _ bp _)]) -> ap' > bp
    where ob' = fst $ matchAsk a ob
          as  = toList $ (askBook ob') @= aid
          bs  = take 1 $ toDescList (Proxy :: Proxy Price) (bidBook ob')

prop_NoEmptyBid :: Bid -> OrderBook -> Bool
prop_NoEmptyBid b@(Order _ _ bp _) ob = let (mb', (_,_)) = runState (matchBid' b asks) (ob, [])
                                         in case mb' of
                                                Just (Order _ _ _ bq') -> bq' > 0
                                                Nothing                -> True
    where asks = takeWhile (\(Order _ _ ap _) -> ap < bp) $
                    toAscList (Proxy :: Proxy Price) (askBook ob)

prop_NoEscapedAsks :: Order -> OrderBook -> Bool
prop_NoEscapedAsks b@(Order bid _ _ _) ob = case (as, bs) of
                                                (                [],                   _) -> True
                                                (                 _,                  []) -> True
                                                ([(Order _ _ ap _)], [(Order _ _ bp' _)]) -> ap > bp'
    where ob' = fst $ matchBid b ob
          as  = take 1 $ toAscList (Proxy :: Proxy Price) (askBook ob')
          bs  = toList $ (bidBook ob') @= bid

instance QC.Arbitrary Order where
    arbitrary = Order <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

genPriceAbove :: Price -> QC.Gen Order
genPriceAbove l = Order <$> QC.arbitrary <*> QC.arbitrary <*> QC.choose (l, maxBound) <*> QC.arbitrary

genPriceBelow :: Price -> QC.Gen Order
genPriceBelow h = Order <$> QC.arbitrary <*> QC.arbitrary <*> QC.choose (minBound, h) <*> QC.arbitrary

instance QC.Arbitrary OrderBook where
    arbitrary = do split <- QC.arbitrary
                   asks  <- QC.listOf $ genPriceAbove split
                   bids  <- QC.listOf $ genPriceBelow split
                   return $ OrderBook (fromList asks) (fromList bids)
