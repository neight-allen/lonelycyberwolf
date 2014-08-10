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
                                                                      call)
import           Control.Distributed.Process.Platform.ManagedProcess
import           Data.Binary
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
newtype Price    = Price   { unPrice :: Int64}      deriving (Eq, Ord, Num, Enum, Typeable, Data, Binary)
newtype Quantity = Quantity { unQuantity :: Int32 } deriving (Eq, Ord, Num, Enum, Typeable, Data, Binary)

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

type AskId = OrderId
type BidId = OrderId

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

postAsk' :: OrderBook -> PostAsk -> Process (ProcessReply OrderId OrderBook)
postAsk' ob@OrderBook{..} (PostAsk p q) = do
        oid <- liftIO nextRandom

        {-let (fo, ml, nob) = attemptFill (Order (OrderId oid) p q) (<) ob-}
        {-let filledBids = attemptFill (Order (OrderId oid) p q) $-}
                            {-toDescList (Proxy :: Proxy Price) bidBook-}


        reply (OrderId oid) ob

postBid' :: OrderBook -> PostBid -> Process (ProcessReply OrderId OrderBook)
postBid' ob (PostBid p q) = do
        oid <- liftIO nextRandom
        reply (OrderId oid) ob

cancelAsk' :: OrderBook -> CancelAsk -> Process (ProcessReply Bool OrderBook)
cancelAsk' ob (CancelAsk oid) = do
        let ab = deleteIx oid (askBook ob)
        reply False (ob { askBook = ab })

cancelBid' :: OrderBook -> CancelBid -> Process (ProcessReply Bool OrderBook)
cancelBid' ob (CancelBid oid) = do
        let bb = deleteIx oid (bidBook ob)
        reply False (ob { bidBook = bb })

---------------
-- Utilities --
---------------

{-attemptFill :: Order -> [Order] -> (Order, [Order])-}
{-attemptFill = undefined-}

{-attemptFill :: Order -> Operation Price -> OrderBook -> (Maybe Order, [Match], OrderBook)-}
{-attemptFill = undefined-}

{-fillAsk :: Order -> OrderBook -> (Maybe Order, [Match], OrderBook)-}
{-fillAsk o ob = undefined-}
    {-where nextBid = take 1 $ toDescList (Proxy :: Proxy Price) (bidBook ob)-}
