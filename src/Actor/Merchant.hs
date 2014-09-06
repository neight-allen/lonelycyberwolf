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

type AskId = OrderId
type BidId = OrderId

data NotifyBid = NotifyBid AskId MerchantId BidId Price Quantity deriving (Typeable, Generic)
instance Binary NotifyBid

----

notifyBid :: MerchantId -> AskId -> MerchantId -> BidId -> Price -> Quantity -> Process ()
notifyBid MerchantId{..} aid mid bid p q = call unMerchantId $ NotifyBid aid mid bid p q
