{-# LANGUAGE RecordWildCards #-}

module Actor.Merchant
    ( NotifyBid (..)

    , notifyBid
    ) where

import           Control.Distributed.Process                         hiding
                                                                      (call)
import           Control.Distributed.Process.Platform.ManagedProcess
import           Data.Binary                                         (Binary)
import           Data.Data
import           Data.Typeable
import           GHC.Generics

import           Actor.Types

type BidId = OrderId

data NotifyBid = NotifyBid MerchantId BidId Price Quantity deriving (Typeable, Generic)
instance Binary NotifyBid

----

notifyBid :: MerchantId -> MerchantId -> BidId -> Price -> Quantity -> Process ()
notifyBid MerchantId{..} m oid p q = call unMerchantId $ NotifyBid m oid p q
