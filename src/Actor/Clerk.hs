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

data PostAsk = PostAsk MerchantId Price Quantity deriving (Typeable, Generic)
instance Binary PostAsk

data PostBid = PostBid MerchantId Price Quantity deriving (Typeable, Generic)
instance Binary PostBid

data CancelAsk = CancelAsk OrderId deriving (Typeable, Generic)
instance Binary CancelAsk

data CancelBid = CancelBid OrderId deriving (Typeable, Generic)
instance Binary CancelBid

----

postAsk :: ClerkId -> MerchantId -> Price -> Quantity -> Process OrderId
postAsk ClerkId{..} m p q = call unClerkId $ PostAsk m p q

postBid :: ClerkId -> MerchantId -> Price -> Quantity -> Process OrderId
postBid ClerkId{..} m p q = call unClerkId $ PostBid m p q

cancelAsk :: ClerkId -> OrderId -> Process ()
cancelAsk ClerkId{..} o = call unClerkId $ CancelAsk o

cancelBid :: ClerkId -> OrderId -> Process ()
cancelBid ClerkId{..} o = call unClerkId $ CancelBid o
