{-# LANGUAGE RecordWildCards #-}

module Actor.Merchant
    ( NotifyAsk (..)
    , NotifyBid (..)
    , NotifyEscrow (..)

    , notifyAsk
    , notifyBid
    , notifyEscrow
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

data NotifyAsk = NotifyAsk Match deriving (Typeable, Generic)
instance Binary NotifyAsk

data NotifyBid = NotifyBid Match deriving (Typeable, Generic)
instance Binary NotifyBid

data NotifyEscrow = NotifyEscrow BidId EscrowId deriving (Typeable, Generic)
instance Binary NotifyEscrow

----

notifyAsk :: MerchantId -> Match -> Process ()
notifyAsk MerchantId{..} m = call unMerchantId $ NotifyAsk m

notifyBid :: MerchantId -> Match -> Process ()
notifyBid MerchantId{..} m = call unMerchantId $ NotifyBid m

notifyEscrow :: MerchantId -> BidId -> EscrowId -> Process ()
notifyEscrow MerchantId{..} bid eid = call unMerchantId $ NotifyEscrow bid eid
