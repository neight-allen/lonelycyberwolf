{-# LANGUAGE RecordWildCards #-}

module Actor.Merchant.Implementation
    ( Inventory (..)
    , merchant
    ) where

import           Control.Distributed.Process                         hiding
                                                                      (Match)
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time
import           Data.Data
import           Data.IxSet
import           Data.Set
import           Data.Text
import           Data.Typeable
import           Data.UUID
import           Data.UUID.V4
import           GHC.Generics

import           Actor.Clerk
import           Actor.Escrow
import           Actor.Merchant
import           Actor.Types

data CommodityReal = CommodityReal CommodityId Quantity deriving (Typeable, Data, Generic, Show)

instance Eq CommodityReal where
    (CommodityReal id0 _) == (CommodityReal id1 _) = id0 == id1

instance Ord CommodityReal where
    compare (CommodityReal _ q0) (CommodityReal _ q1) = compare q0 q1

type Inventory = IxSet CommodityReal

data Merchant = Merchant
        { merchantName   :: !Text
        , merchantUUID   :: !UUID
        , merchantWealth :: !Price
        , inventory      :: !Inventory
        , openAsks       :: !(IxSet Order)
        } deriving (Show)

----

merchant :: ProcessDefinition Merchant
merchant = ProcessDefinition
        { apiHandlers            = [ handleCall notifyAsk'
                                   , handleCall notifyBid'
                                   ]
        , infoHandlers           = []
        , exitHandlers           = []
        , timeoutHandler         = \s _ -> continue s
        , shutdownHandler        = \_ _ -> return ()
        , unhandledMessagePolicy = Drop
        }

{- Bidding Merchant waits for an escrow to come in from the Asker -}
notifyAsk' :: Merchant -> NotifyAsk -> Process (ProcessReply () Merchant)
notifyAsk' s (NotifyAsk epid m) = do
        noReply_ s

{- Asking Merchant creates the escrow, and passes the id to the bidder -}
notifyBid' :: Merchant -> NotifyBid -> Process (ProcessReply () Merchant)
notifyBid' s (NotifyBid epid m) = do
        noReply_ s
{-notifyBid' s (NotifyBid m@(Match aid amid bid bmid p q)) = do-}
        {-pid <- spawnLocal $ serve Holding init escrow-}
        {-let eid = EscrowId pid-}
        {-notifyEscrow bmid bid eid-}
        {-commitAsk eid m-}
        {-noReply_ s-}
    {-where init h = return $ InitOk h Infinity-}
