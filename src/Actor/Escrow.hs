{-# LANGUAGE RecordWildCards #-}

module Actor.Escrow
    ( CommitAsk (..)
    , CommitBid (..)

    , commitAsk
    , commitBid
    ) where

import           Control.Distributed.Process                         hiding
                                                                      (call)
import           Control.Distributed.Process.Platform.ManagedProcess
import           Data.Binary
import           Data.Data
import           Data.Typeable
import           GHC.Generics

import           Actor.Types

data CommitAsk = CommitAsk deriving (Typeable, Generic)
instance Binary CommitAsk

data CommitBid = CommitBid deriving (Typeable, Generic)
instance Binary CommitBid

----

commitAsk :: EscrowId -> Process ()
commitAsk EscrowId{..} = call unEscrowId $ CommitAsk

commitBid :: EscrowId -> Process ()
commitBid EscrowId{..} = call unEscrowId $ CommitBid
