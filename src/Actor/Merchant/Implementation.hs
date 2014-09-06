{-# LANGUAGE RecordWildCards #-}

module Actor.Merchant.Implementation
    ( Inventory (..)
    , merchant
    ) where

import           Control.Distributed.Process
import           Control.Distributed.Process.Platform.ManagedProcess
import           Data.Data
import           Data.IxSet
import           Data.Set
import           Data.Text
import           Data.Typeable
import           Data.UUID
import           Data.UUID.V4
import           GHC.Generics

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
        { apiHandlers            = [ handleCall notifyBid' ]
        , infoHandlers           = []
        , exitHandlers           = []
        , timeoutHandler         = \s _ -> continue s
        , shutdownHandler        = \_ _ -> return ()
        , unhandledMessagePolicy = Drop
        }

notifyBid' :: Merchant -> NotifyBid -> Process (ProcessReply () Merchant)
notifyBid' m@Merchant{..} (NotifyBid aid mid bid p q) = do

        noReply_ m

{-notifyBid' i _ = reply True i-}
