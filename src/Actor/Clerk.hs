{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Actor.Clerk
    ( OrderId , Price, Quantity
    , ClerkId (..)

    , postAsk
    , postBid
    , cancelAsk
    , cancelBid

    , clerk
    ) where

import           Control.Distributed.Process                         hiding
                                                                      (Match,
                                                                      call,
                                                                      match)
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Monad.State
import           Data.Binary                                         (Binary)
import           Data.Data
import           Data.IxSet
import           Data.Typeable
import           Data.UUID
import           Data.UUID.V4
import           GHC.Generics
import           GHC.Int

---------------
-- Interface --
---------------

newtype OrderId  = OrderId { unOrderId :: UUID }    deriving (Eq, Ord, Typeable, Data, Binary)
newtype Price    = Price   { unPrice :: Int64 }     deriving (Eq, Ord, Num, Real, Integral, Enum, Typeable, Data, Binary)
newtype Quantity = Quantity { unQuantity :: Int32 } deriving (Eq, Ord, Num, Real, Integral, Enum, Typeable, Data, Binary)

newtype ClerkId = ClerkId { unClerkId :: ProcessId } deriving (Eq, Ord)

data PostAsk = PostAsk Price Quantity deriving (Typeable, Generic)
instance Binary PostAsk

data PostBid = PostBid Price Quantity deriving (Typeable, Generic)
instance Binary PostBid

data CancelAsk = CancelAsk OrderId deriving (Typeable, Generic)
instance Binary CancelAsk

data CancelBid = CancelBid OrderId deriving (Typeable, Generic)
instance Binary CancelBid

----

postAsk :: ClerkId -> Price -> Quantity -> Process OrderId
postAsk ClerkId{..} p q = call unClerkId $ PostAsk p q

postBid :: ClerkId -> Price -> Quantity -> Process OrderId
postBid ClerkId{..} p q = call unClerkId $ PostBid p q

cancelAsk :: ClerkId -> OrderId -> Process ()
cancelAsk ClerkId{..} o = call unClerkId $ CancelAsk o

cancelBid :: ClerkId -> OrderId -> Process ()
cancelBid ClerkId{..} o = call unClerkId $ CancelBid o

--------------------
-- Implementation --
--------------------

type Ask = Order
type Bid = Order

type AskId = OrderId
type BidId = OrderId

type BookTrans = (IxSet Order -> IxSet Order)

data Match = Match AskId BidId Price Quantity

data Order = Order OrderId Price Quantity deriving (Typeable, Data)

instance Eq Order where
    (Order id0 _ _) == (Order id1 _ _) = id0 == id1

instance Ord Order where
    compare (Order _ p0 _) (Order _ p1 _) = compare p0 p1

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
postAsk' ob (PostAsk p q)
    | p > 0 && q > 0 = do
        oid <- liftIO nextRandom

        let (a, trans, matches) = match (Order (OrderId oid) p q) $
                                    takeWhile (\(Order _ bp _) -> bp > p) $
                                        toDescList (Proxy :: Proxy Price) (bidBook ob)

        let bb  = foldl (\bb' t -> t bb') (bidBook ob) trans
            ob' = ob { bidBook = bb }

        {-TODO: Emit messages-}

        case a of
            Just a'@(Order aid _ _) -> reply (Just aid) (ob' { askBook = insert a' (askBook ob') })
            Nothing                 -> reply Nothing    ob'
    | otherwise      = reply Nothing ob

postBid' :: OrderBook -> PostBid -> Process (ProcessReply (Maybe OrderId) OrderBook)
postBid' ob (PostBid p q)
    | p > 0 && q > 0 = do
        oid <- liftIO nextRandom

        let (b, trans, matches) = match (Order (OrderId oid) p q) $
                                    takeWhile (\(Order _ ap _) -> ap < p) $
                                        toAscList (Proxy :: Proxy Price) (askBook ob)

        let ab  = foldl (\ab' t -> t ab') (askBook ob) trans
            ob' = ob { askBook = ab }

        {-TODO: Emit messages-}

        case b of
            Just b'@(Order bid _ _) -> reply (Just bid) (ob' { bidBook = insert b' (bidBook ob') })
            Nothing                 -> reply Nothing ob'
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
matchQ (Order fid fp fq) (Order oid op oq) =
        case compare fq oq of
            LT -> (Nothing                      , updateIx oid (Order oid op (oq - fq)), Match fid oid (med fp op) fq)
            GT -> (Just (Order fid fp (fq - oq)), deleteIx oid                         , Match fid oid (med fp op) oq)
            EQ -> (Nothing                      , deleteIx oid                         , Match fid oid (med fp op) oq)
    where med p0 p1 = (p0 + p1) `div` 2
