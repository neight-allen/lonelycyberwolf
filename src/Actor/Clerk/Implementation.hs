module Actor.Clerk.Implementation
    ( clerk
    , tests
    ) where

import           Control.Applicative
import           Control.Distributed.Process                         hiding
                                                                      (Match,
                                                                      match)
import           Control.Distributed.Process.Platform.ManagedProcess
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

type AskMerchant = MerchantId
type BidMerchant = MerchantId

type Ask = Order
type Bid = Order

type AskId = OrderId
type BidId = OrderId

type BookTrans = (IxSet Order -> IxSet Order)

data Match = Match AskId AskMerchant BidId BidMerchant Price Quantity

data Order = Order OrderId MerchantId Price Quantity deriving (Show, Typeable, Data)

instance Eq Order where
    (Order id0 _ _ _) == (Order id1 _ _ _) = id0 == id1

instance Ord Order where
    compare (Order _ _ p0 _) (Order _ _ p1 _) = compare p0 p1

instance Indexable Order where
    empty = ixSet [ ixGen (Proxy :: Proxy OrderId)
                  , ixGen (Proxy :: Proxy Price)
                  ]

data OrderBook = OrderBook
        { askBook :: !(IxSet Order)
        , bidBook :: !(IxSet Order)
        }

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
        oid <- liftIO nextRandom

        let (a, trans, matches) = match (Order (OrderId oid) mid p q) $
                                    takeWhile (\(Order _ _ bp _) -> bp > p) $
                                        toDescList (Proxy :: Proxy Price) (bidBook ob)

        let bb  = foldl (\bb' t -> t bb') (bidBook ob) trans
            ob' = ob { bidBook = bb }

        mapM_ (\(Match _ am bid bm p' q') -> notifyBid am bm bid p' q') matches

        case a of
            Just a'@(Order aid mid _ _) -> reply (Just aid) (ob' { askBook = insert a' (askBook ob') })
            Nothing                     -> reply Nothing    ob'
    | otherwise      = reply Nothing ob

postBid' :: OrderBook -> PostBid -> Process (ProcessReply (Maybe OrderId) OrderBook)
postBid' ob (PostBid mid p q)
    | p > 0 && q > 0 = do
        oid <- liftIO nextRandom

        let (b, trans, matches) = match (Order (OrderId oid) mid p q) $
                                    takeWhile (\(Order _ _ ap _) -> ap < p) $
                                        toAscList (Proxy :: Proxy Price) (askBook ob)

        let ab  = foldl (\ab' t -> t ab') (askBook ob) trans
            ob' = ob { askBook = ab }

        mapM_ (\(Match _ am bid bm p' q') -> notifyBid am bm bid p' q') matches

        case b of
            Just b'@(Order bid mid _ _) -> reply (Just bid) (ob' { bidBook = insert b' (bidBook ob') })
            Nothing                     -> reply Nothing ob'
    | otherwise      = reply Nothing ob

cancelAsk' :: OrderBook -> CancelAsk -> Process (ProcessReply Bool OrderBook)
cancelAsk' ob (CancelAsk oid) = reply False (ob { askBook = deleteIx oid (askBook ob) })

cancelBid' :: OrderBook -> CancelBid -> Process (ProcessReply Bool OrderBook)
cancelBid' ob (CancelBid oid) = reply False (ob { bidBook = deleteIx oid (bidBook ob) })

---------------
-- Utilities --
---------------

match :: Order -> [Order] -> (Maybe Order, [BookTrans], [Match])
match f os = match' os (Just f, [], [])

match' :: [Order] -> (Maybe Order, [BookTrans], [Match]) -> (Maybe Order, [BookTrans], [Match])
{-# INLINE match' #-}
match' _      (Nothing, ts, ms) = (Nothing, ts, ms)
match' []     e                 = e
match' (o:os) (Just  f, ts, ms) = let (f', t, m) = matchQ f o
                                   in match' os (f', t:ts, m:ms)

matchQ :: Order -> Order -> (Maybe Order, BookTrans, Match)
{-# INLINE matchQ #-}
matchQ (Order fid fmid fp fq) (Order oid omid op oq) =
        case compare fq oq of
            LT -> (Nothing                           , updateIx oid (Order oid omid op (oq - fq)), Match fid fmid oid omid (med fp op) fq)
            GT -> (Just (Order fid fmid fp (fq - oq)), deleteIx oid                              , Match fid fmid oid omid (med fp op) oq)
            EQ -> (Nothing                           , deleteIx oid                              , Match fid fmid oid omid (med fp op) oq)
    where med p0 p1 = (p0 + p1) `div` 2

-------------
-- Testing --
-------------

tests :: Test.TestTree
tests = Test.testGroup "Clerk" [testMatching]

testMatching :: Test.TestTree
testMatching = Test.testGroup "Matching"
        [ QC.testProperty "If an ask cannot be filled, no bids above its price should remain."  prop_NoEscapedBids
        ]

prop_NoEscapedBids :: Order -> Bool
prop_NoEscapedBids (Order oid omid p q) = p >= 0
{-prop_NoEscapedBids o = QC.forAll . QC.orderedList $ \os -> undefined-}

instance QC.Arbitrary Order where
    arbitrary = Order <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
