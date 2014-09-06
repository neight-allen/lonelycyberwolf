module Actor.Clerk.Implementation
    ( clerk
    , tests
    ) where

import           Control.Applicative
import           Control.Distributed.Process                         hiding
                                                                      (Match,
                                                                      match)
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Monad.State
import           Data.Data                                           hiding
                                                                      (Proxy)
import           Data.IxSet
import           Data.Typeable                                       hiding
                                                                      (Proxy)
import           Data.UUID.V4

import qualified Test.Tasty                                          as Test
import qualified Test.Tasty.QuickCheck                               as QC

import           Actor.Clerk
import           Actor.Merchant
import           Actor.Types

type BookTrans = (IxSet Order -> IxSet Order)

type MatchT = State (OrderBook, [Match]) (Maybe Order)

----

clerk :: ProcessDefinition OrderBook
clerk = ProcessDefinition
        { apiHandlers            = [ handleCall postAsk'
                                   , handleCall postBid'
                                   , handleCall cancelAsk'
                                   , handleCall cancelBid'
                                   ]
        , infoHandlers           = []
        , exitHandlers           = []
        , timeoutHandler         = \s _ -> continue s
        , shutdownHandler        = \_ _ -> return ()
        , unhandledMessagePolicy = Drop
        }

postAsk' :: OrderBook -> PostAsk -> Process (ProcessReply (Maybe OrderId) OrderBook)
postAsk' ob (PostAsk mid p q)
    | p > 0 && q > 0 = do
        uuid <- liftIO nextRandom
        let oid       = OrderId uuid
            (ob', ms) = matchAsk (Order oid mid p q) ob
        mapM_ (\m@(Match _ am _ bm _ _) -> notifyAsk am m >> notifyBid bm m) ms
        reply (Just oid) ob'
    | otherwise      = reply Nothing ob

postBid' :: OrderBook -> PostBid -> Process (ProcessReply (Maybe OrderId) OrderBook)
postBid' ob (PostBid mid p q)
    | p > 0 && q > 0 = do
        uuid <- liftIO nextRandom
        let oid       = OrderId uuid
            (ob', ms) = matchBid (Order oid mid p q) ob
        mapM_ (\m@(Match _ am _ bm _ _) -> notifyAsk am m >> notifyBid bm m) ms
        reply (Just oid) ob'
    | otherwise      = reply Nothing ob

cancelAsk' :: OrderBook -> CancelAsk -> Process (ProcessReply Bool OrderBook)
cancelAsk' ob (CancelAsk oid) = reply False (ob { askBook = deleteIx oid (askBook ob) })

cancelBid' :: OrderBook -> CancelBid -> Process (ProcessReply Bool OrderBook)
cancelBid' ob (CancelBid oid) = reply False (ob { bidBook = deleteIx oid (bidBook ob) })

---------------
-- Utilities --
---------------

-- Ask --

matchAsk :: Ask -> OrderBook -> (OrderBook, [Match])
{-# INLINABLE matchAsk #-}
matchAsk a@(Order _ _ ap _) ob = let (ma', (ob', ms)) = runState (matchAsk' a bids) (ob, [])
                                  in case ma' of
                                        Just a' -> (ob' { askBook = insert a' (askBook ob') }, ms)
                                        Nothing -> (ob', ms)
    where bids = takeWhile (\(Order _ _ bp _) -> ap < bp) $
                    toDescList (Proxy :: Proxy Price) (bidBook ob)

matchAsk' :: Ask -> [Bid] -> MatchT
{-# INLINE matchAsk' #-}
matchAsk' a                                         [] = return $ Just a
matchAsk' a@(Order _ _ ap _) (b@(Order bid _ bp _):bs) =
        let (ma', mb', m) = fill a b
         in case mb' of
                Just b' -> get >>= (\(ob, ms) -> put (ob { bidBook = updateIx bid b' (bidBook ob) }, m:ms)) >> handleAsk ma'
                Nothing -> get >>= (\(ob, ms) -> put (ob { bidBook = deleteIx bid (bidBook ob) }, m:ms)) >> handleAsk ma'
    where handleAsk ma' = case ma' of Just a' -> matchAsk' a' bs
                                      Nothing -> return Nothing

-- Bid --

matchBid :: Bid -> OrderBook -> (OrderBook, [Match])
{-# INLINABLE matchBid #-}
matchBid b@(Order _ _ bp _) ob = let (mb', (ob', ms)) = runState (matchBid' b asks) (ob, [])
                                  in case mb' of
                                        Just b' -> (ob' { bidBook = insert b' (bidBook ob') }, ms)
                                        Nothing -> (ob', ms)
    where asks = takeWhile (\(Order _ _ ap _) -> ap < bp) $
                    toAscList (Proxy :: Proxy Price) (askBook ob)

matchBid' :: Bid -> [Ask] -> MatchT
{-# INLINE matchBid' #-}
matchBid' b                                         [] = return $ Just b
matchBid' b@(Order _ _ bp _) (a@(Order aid _ ap _):as) =
        let (ma', mb', m) = fill a b
         in case ma' of
                Just a' -> get >>= (\(ob, ms) -> put (ob { askBook = updateIx aid a' (askBook ob) }, m:ms)) >> handleBid mb'
                Nothing -> get >>= (\(ob, ms) -> put (ob { askBook = deleteIx aid (askBook ob) }, m:ms)) >> handleBid mb'
    where handleBid mb' = case mb' of Just b' -> matchBid' b' as
                                      Nothing -> return Nothing

-- Util --

fill :: Ask -> Bid -> (Maybe Ask, Maybe Bid, Match)
{-# INLINE fill #-}
fill (Order aid amid ap aq) (Order bid bmid bp bq) =
        case compare aq bq of
            LT -> (                           Nothing, Just (Order bid bmid bp (bq - aq)), Match aid amid bid bmid (med ap bp) aq)
            EQ -> (                           Nothing,                            Nothing, Match aid amid bid bmid (med ap bp) bq)
            GT -> (Just (Order aid amid ap (aq - bq)),                            Nothing, Match aid amid bid bmid (med ap bp) bq)
    where med p0 p1 = (p0 + p1) `div` 2

-------------
-- Testing --
-------------

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
