{-# LANGUAGE RecordWildCards #-}

module Actor.Clerk
    ( PostAsk (..)
    , PostBid (..)
    , CancelAsk (..)
    , CancelBid (..)

    , postAsk
    , postBid
    , cancelAsk
    , cancelBid
    ) where

import           Control.Distributed.Process                         hiding
                                                                      (call)
import           Control.Distributed.Process.Platform.ManagedProcess
import           Data.Binary                                         (Binary)
import           Data.Data
import           Data.Typeable
import           GHC.Generics

import           Actor.Types

data PostAsk = PostAsk MerchantPid Price Quantity deriving (Typeable, Generic)
instance Binary PostAsk

data PostBid = PostBid MerchantPid Price Quantity deriving (Typeable, Generic)
instance Binary PostBid

data CancelAsk = CancelAsk OrderId deriving (Typeable, Generic)
instance Binary CancelAsk

data CancelBid = CancelBid OrderId deriving (Typeable, Generic)
instance Binary CancelBid

----

postAsk :: ClerkPid -> MerchantPid -> Price -> Quantity -> Process OrderId
postAsk ClerkPid{..} m p q = call unClerkPid $ PostAsk m p q

postBid :: ClerkPid -> MerchantPid -> Price -> Quantity -> Process OrderId
postBid ClerkPid{..} m p q = call unClerkPid $ PostBid m p q

cancelAsk :: ClerkPid -> OrderId -> Process ()
cancelAsk ClerkPid{..} o = call unClerkPid $ CancelAsk o

cancelBid :: ClerkPid -> OrderId -> Process ()
cancelBid ClerkPid{..} o = call unClerkPid $ CancelBid o
