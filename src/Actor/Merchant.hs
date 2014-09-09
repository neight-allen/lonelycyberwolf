{-# LANGUAGE RecordWildCards #-}

module Actor.Merchant
    ( NotifyAsk (..)
    , NotifyBid (..)

    , notifyAsk
    , notifyBid
    ) where

import           Control.Distributed.Process                         hiding
                                                                      (Match,
                                                                      call)
import           Control.Distributed.Process.Platform.ManagedProcess
import           Data.Binary                                         (Binary)
import           Data.Data
import           Data.Typeable
import           GHC.Generics

import           Actor.Types

data NotifyAsk = NotifyAsk EscrowId Match deriving (Typeable, Generic)
instance Binary NotifyAsk

data NotifyBid = NotifyBid EscrowId Match deriving (Typeable, Generic)
instance Binary NotifyBid

----

notifyAsk :: MerchantId -> EscrowId -> Match -> Process ()
notifyAsk MerchantId{..} eid m = call unMerchantId $ NotifyAsk eid m

notifyBid :: MerchantId -> EscrowId -> Match -> Process ()
notifyBid MerchantId{..} eid m = call unMerchantId $ NotifyBid eid m
