module Actor.Clerk.Implementation where

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
import           Data.UUID
import           Data.UUID.V4

import           Actor.Clerk
import           Actor.Merchant
import           Actor.Types

type BookTrans = (IxSet Order -> IxSet Order)

type MatchT = State (OrderBook, [Match]) (Maybe Order)

data OrderBook = OrderBook
        { askBook :: !(IxSet Order)
        , bidBook :: !(IxSet Order)
        } deriving (Show)

data Clerk = Clerk ClerkId EscrowId EscrowPid OrderBook deriving (Show)

----

clerk :: ProcessDefinition Clerk
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

postAsk' :: Clerk -> PostAsk -> Process (ProcessReply (Maybe OrderId) Clerk)
postAsk' (Clerk cid eid epid ob) (PostAsk mid p q)
    | p > 0 && q > 0 = do
        uuid <- liftIO nextRandom
        let oid       = OrderId uuid
            (ob', ms) = matchAsk (Order oid mid p q) ob
        mapM_ (\m@(Match _ am _ bm _ _) -> notifyAsk am epid m >> notifyBid bm epid m) ms
        reply (Just oid) (Clerk cid eid epid ob')
    | otherwise      = reply Nothing (Clerk cid eid epid ob)

postBid' :: Clerk -> PostBid -> Process (ProcessReply (Maybe OrderId) Clerk)
postBid' (Clerk cid eid epid ob) (PostBid mid p q)
    | p > 0 && q > 0 = do
        uuid <- liftIO nextRandom
        let oid       = OrderId uuid
            (ob', ms) = matchBid (Order oid mid p q) ob
        mapM_ (\m@(Match _ am _ bm _ _) -> notifyAsk am epid m >> notifyBid bm epid m) ms
        reply (Just oid) (Clerk cid eid epid ob')
    | otherwise      = reply Nothing (Clerk cid eid epid ob)

cancelAsk' :: Clerk -> CancelAsk -> Process (ProcessReply Bool Clerk)
cancelAsk' (Clerk cid eid epid ob) (CancelAsk oid) =
        reply False (Clerk cid eid epid (ob { askBook = deleteIx oid (askBook ob) }))

cancelBid' :: Clerk -> CancelBid -> Process (ProcessReply Bool Clerk)
cancelBid' (Clerk cid eid epid ob) (CancelBid oid) =
        reply False (Clerk cid eid epid (ob { bidBook = deleteIx oid (bidBook ob) }))

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
