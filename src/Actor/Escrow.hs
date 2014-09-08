{-# LANGUAGE RecordWildCards #-}

module Actor.Escrow
    ( CommitAsk (..)
    , CommitBid (..)

    , commitAsk
    , commitBid
    ) where

import           Control.Distributed.Process                         hiding
                                                                      (Match,
                                                                      call)
import           Control.Distributed.Process.Platform.ManagedProcess
import           Data.Binary
import           Data.Data
import           Data.Typeable
import           GHC.Generics

import           Actor.Types

data CommitAsk = CommitAsk Match deriving (Typeable, Generic)
instance Binary CommitAsk

data CommitBid = CommitBid Match deriving (Typeable, Generic)
instance Binary CommitBid

----

commitAsk :: EscrowId -> Match -> Process ()
commitAsk EscrowId{..} m = call unEscrowId $ CommitAsk m

commitBid :: EscrowId -> Match -> Process ()
commitBid EscrowId{..} m = call unEscrowId $ CommitBid m
