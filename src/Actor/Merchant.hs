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

data NotifyAsk = NotifyAsk Match deriving (Typeable, Generic)
instance Binary NotifyAsk

data NotifyBid = NotifyBid Match deriving (Typeable, Generic)
instance Binary NotifyBid

----

notifyAsk :: MerchantId -> Match -> Process ()
notifyAsk MerchantId{..} m = call unMerchantId $ NotifyAsk m

notifyBid :: MerchantId -> Match -> Process ()
notifyBid MerchantId{..} m = call unMerchantId $ NotifyBid m
