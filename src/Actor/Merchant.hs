{-# LANGUAGE RecordWildCards #-}

module Actor.Merchant
    ( NotifyAsk (..)
    , NotifyBid (..)
    , ConfirmTransaction (..)

    , notifyAsk
    , notifyBid
    , confirmTransaction
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

data NotifyAsk = NotifyAsk EscrowPid Match deriving (Typeable, Generic)
instance Binary NotifyAsk

data NotifyBid = NotifyBid EscrowPid Match deriving (Typeable, Generic)
instance Binary NotifyBid

data ConfirmTransaction = ConfirmTransaction Match deriving (Typeable, Generic)
instance Binary ConfirmTransaction

----

notifyAsk :: MerchantPid -> EscrowPid -> Match -> Process ()
notifyAsk MerchantPid{..} eid m = call unMerchantPid $ NotifyAsk eid m

notifyBid :: MerchantPid -> EscrowPid -> Match -> Process ()
notifyBid MerchantPid{..} eid m = call unMerchantPid $ NotifyBid eid m

confirmTransaction :: MerchantPid -> Match -> Process ()
confirmTransaction MerchantPid{..} m = call unMerchantPid $ ConfirmTransaction m
