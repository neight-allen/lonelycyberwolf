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

commitAsk :: EscrowPid -> Match -> Process ()
commitAsk EscrowPid{..} m = call unEscrowPid $ CommitAsk m

commitBid :: EscrowPid -> Match -> Process ()
commitBid EscrowPid{..} m = call unEscrowPid $ CommitBid m
